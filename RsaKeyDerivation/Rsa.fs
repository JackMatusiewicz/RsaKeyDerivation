namespace RsaKeyDerivation

open Function
open System.Security.Cryptography
open System.Collections.Generic
open System.Linq
open State
open System.Numerics

module Rsa =

    type RsaParameters = {
        d : bigint
        p : bigint
        q : bigint
        e : bigint
        n : bigint
    }

    let private toBigEndianByteArray (p : bigint) : byte[] =
        p.ToByteArray()
            |> Array.rev
            |> Seq.skipWhile ((=) (byte 0))
            |> Array.ofSeq

    //Used to pad any values that aren't the correct length.
    let private ensureCorrectSize (size : int) (p : byte[]) : byte[] =
        let littleEndianData = Array.rev p
        let buffer = Array.create size (byte 0)
        System.Array.Copy(littleEndianData, buffer, littleEndianData.Length)
        buffer |> Array.rev

    let private createParams ((Prime p) : Prime) ((Prime q) : Prime) : RsaParameters option =
        let n = p * q
        let lambdaN = Math.lcm (p - (bigint 1)) (q - (bigint 1))
        let e = (bigint 65537)
        let d = Math.modularInverse e lambdaN
        match d with
        | None -> None
        | Some dVal -> Some <| {d = dVal; p = p; q = q; e = e; n = n}

    let private fillParameters (keySizeInBits : int) (rsa : RsaParameters) =
        let primeSizeInBytes = keySizeInBits / 16
        let mutable rsaParams = new RSAParameters()
        rsaParams.DP <- rsa.d % (rsa.p - (bigint 1))
                        |> toBigEndianByteArray |> ensureCorrectSize primeSizeInBytes
        rsaParams.DQ <- rsa.d % (rsa.q - (bigint 1))
                        |> toBigEndianByteArray |> ensureCorrectSize primeSizeInBytes
        rsaParams.Exponent <- rsa.e |> toBigEndianByteArray
        rsaParams.P <- rsa.p |> toBigEndianByteArray
        rsaParams.Q <- rsa.q |> toBigEndianByteArray
        rsaParams.Modulus <- rsa.n |> toBigEndianByteArray
        rsaParams.InverseQ <- BigInteger.ModPow(rsa.q, (rsa.p - (bigint 2)), rsa.p)
                                |> toBigEndianByteArray |> ensureCorrectSize primeSizeInBytes
        rsaParams.D <- rsa.d |> toBigEndianByteArray |> ensureCorrectSize (rsaParams.Modulus.Length)
        rsaParams

    let rec findKey (populateKey : RsaParameters -> RSAParameters)
            (keyGen : State<Csprng, RsaParameters option>) : State<Csprng, RSAParameters> = state {
        let! potentialKey = keyGen
        match potentialKey with
        | None -> return! findKey populateKey keyGen
        | Some p -> return populateKey p
    }

    let createKey (keySizeInBits : int) (numberOfPrimeChecks : int) : State<Csprng, RSAParameters> =
        let numberOfBlocks = keySizeInBits / 16 / 16
        let genRandNum = Csprng.random numberOfBlocks
        let findNextPrime = Prime.findPrime numberOfPrimeChecks
        let generateRandomPrime = genRandNum >>= findNextPrime
        findKey (fillParameters keySizeInBits) (createParams <!> generateRandomPrime <*> generateRandomPrime)
