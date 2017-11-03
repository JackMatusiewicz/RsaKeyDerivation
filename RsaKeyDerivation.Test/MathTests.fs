namespace RsaKeyDerivation.Test

module MathTests =
    open NUnit.Framework
    open RsaKeyDerivation
    open RsaKeyDerivation.State

    [<Test>]
    let ``Given random modulus and number, when calculating modular inverse then result is correct``() =
        let mutable csprng = Csprng.create ()
        let genRandNum = Csprng.randomForRsa 4
        let findNextPrime = Prime.findPrime 6
        let generateRandomPrime = genRandNum >>= findNextPrime
        for i in 1 .. 10 do
            let modulus,newCsprng = runState generateRandomPrime csprng
            let a,finalCsprng = runState generateRandomPrime newCsprng
            csprng <- finalCsprng

            let a = (Prime.toBigInt a)
            let modulus = (Prime.toBigInt modulus)
            let modInv = Math.modularInverse a modulus
            match modInv with
            | Some mi ->
                Assert.That((a * mi) % modulus, Is.EqualTo(bigint 1))
            | None -> Assert.Fail("Two primes should have an inverse")