namespace RsaKeyDerivation

module Rsa =
    open Function
    open System.Security.Cryptography
    open System.Collections.Generic
    open System.Linq
    open State
    open System.Numerics

    let private toBigEndianByteArray (p : bigint) : byte[] =
        p.ToByteArray()
            |> Array.rev
            |> Seq.skipWhile ((=) (byte 0))
            |> Array.ofSeq

    let private derive ((Prime p) : Prime) ((Prime q) : Prime) : RSAParameters =
        let n = p * q
        let lambdaN = Math.lcm (p - (bigint 1)) (q - (bigint 1))
        let e = (bigint 65537)
        let d = Math.modInverse e lambdaN

        let mutable rsaParams = new RSAParameters()
        rsaParams.D <- d |> toBigEndianByteArray
        rsaParams.DP <- d % (p - (bigint 1)) |> toBigEndianByteArray
        rsaParams.DQ <- d % (q - (bigint 1)) |> toBigEndianByteArray
        rsaParams.Exponent <- e |> toBigEndianByteArray
        rsaParams.P <- p |> toBigEndianByteArray
        rsaParams.Q <- q |> toBigEndianByteArray
        rsaParams.Modulus <- n |> toBigEndianByteArray
        rsaParams.InverseQ <- BigInteger.ModPow(q, (p - (bigint 2)), p) |> toBigEndianByteArray
        rsaParams

    let createKey (numberOfBlocks : int) (numberOfPrimeChecks : int) : State<Csprng, RSAParameters> =
        let genRandNum = Csprng.randomForRsa numberOfBlocks
        let findNextPrime = Prime.findPrime numberOfPrimeChecks
        let generateRandomPrime = genRandNum >>= findNextPrime
        derive <!> generateRandomPrime <*> generateRandomPrime
