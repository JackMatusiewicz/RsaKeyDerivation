namespace RsaKeyDerivation

module Rsa =
    open Function
    open System.Security.Cryptography
    open System.Collections.Generic
    open System.Linq
    open State
    open System.Numerics

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

    //The issue is that the msb of the modulus is not set to 1.
    let private createParams ((Prime p) : Prime) ((Prime q) : Prime) : RsaParameters option =
        let n = p * q
        let lambdaN = Math.lcm (p - (bigint 1)) (q - (bigint 1))
        let e = (bigint 65537)
        let d = Math.modularInverse e lambdaN
        match d with
        | None -> None
        | Some dVal -> Some <| {d = dVal; p = p; q = q; e = e; n = n}

    let private fillParameters (rsa : RsaParameters) =
        let mutable rsaParams = new RSAParameters()
        rsaParams.D <- rsa.d |> toBigEndianByteArray
        rsaParams.DP <- rsa.d % (rsa.p - (bigint 1)) |> toBigEndianByteArray
        rsaParams.DQ <- rsa.d % (rsa.q - (bigint 1)) |> toBigEndianByteArray
        rsaParams.Exponent <- rsa.e |> toBigEndianByteArray
        rsaParams.P <- rsa.p |> toBigEndianByteArray
        rsaParams.Q <- rsa.q |> toBigEndianByteArray
        rsaParams.Modulus <- rsa.n |> toBigEndianByteArray
        rsaParams.InverseQ <- BigInteger.ModPow(rsa.q, (rsa.p - (bigint 2)), rsa.p) |> toBigEndianByteArray
        rsaParams

    let rec findKey (keyGen : State<Csprng, RsaParameters option>) : State<Csprng, RSAParameters> = state {
        let! potentialKey = keyGen
        match potentialKey with
        | None -> return! findKey keyGen
        | Some p -> return fillParameters p
    }

    let createKey (numberOfBlocks : int) (numberOfPrimeChecks : int) : State<Csprng, RSAParameters> =
        let genRandNum = Csprng.randomForRsa numberOfBlocks
        let findNextPrime = Prime.findPrime numberOfPrimeChecks
        let generateRandomPrime = genRandNum >>= findNextPrime
        findKey (createParams <!> generateRandomPrime <*> generateRandomPrime)