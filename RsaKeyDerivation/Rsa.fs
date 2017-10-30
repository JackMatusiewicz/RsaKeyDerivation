namespace RsaKeyDerivation

module Rsa =
    open Function
    open System.Security.Cryptography
    open System.Collections.Generic
    open System.Linq
    open State

    let private toByteArray (p : bigint) : byte[] =
        p.ToByteArray()
            |> Array.rev
            |> Seq.ofArray
            |> Seq.skipWhile ((=) (byte 0))
            |> Array.ofSeq

    let private derive (primeOne : Prime) (primeTwo : Prime) : RSAParameters =
        let (Prime p) = primeOne
        let (Prime q) = primeTwo
        let n = p * q
        let lambdaN = Math.lcm (p - (bigint 1)) (q - (bigint 1))
        let e = (bigint 65537)
        let d = Math.modInverse e lambdaN

        let mutable rsaParams = new RSAParameters()
        rsaParams.D <- d |> toByteArray
        rsaParams.DP <- d % (p - (bigint 1)) |> toByteArray
        rsaParams.DQ <- d % (q - (bigint 1)) |> toByteArray
        rsaParams.Exponent <- e |> toByteArray
        rsaParams.P <- p |> toByteArray
        rsaParams.Q <- q |> toByteArray
        rsaParams.Modulus <- n |> toByteArray
        rsaParams.InverseQ <- Math.modInverse q p |> toByteArray
        rsaParams

    let createKey (numberOfBlocks : int) (numberOfPrimeChecks : int) : State<Csprng, RSAParameters> =
        let genRandNum = Csprng.randomForRsa numberOfBlocks
        let findNextPrime = Prime.findPrime numberOfPrimeChecks
        let generateRandomPrime = genRandNum >>= findNextPrime
        derive <!> generateRandomPrime <*> generateRandomPrime
