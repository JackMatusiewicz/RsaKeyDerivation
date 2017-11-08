namespace RsaKeyDerivation.Test

open NUnit.Framework
open RsaKeyDerivation
open RsaKeyDerivation.State
open Function
open System.Numerics
open Math

module MathTests =

    let gcd (a : bigint) (b : bigint) =
        BigInteger.GreatestCommonDivisor(a,b)

    [<Test>]
    let ``Given random modulus and number, when calculating modular inverse then result is correct``() =
        let mutable csprng = Csprng.create ()
        let genRandNum = Csprng.random 1
        let findNextPrime = Prime.findPrime 6
        let generateRandomPrime = genRandNum >>= findNextPrime
        for i in 1 .. 200 do
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

    [<Test>]
    [<Repeat(50)>]
    let ``Given two numbers, m and n, the gcd multiplied by lcm equals m * n``() =
        let csprng = Csprng.create ()
        let makeNumbers = makeTuple <!> (Csprng.random 1) <*> (Csprng. random 1)
        let (a,b),_ = runState makeNumbers csprng

        Assert.That((gcd a b) * (lcm a b), Is.EqualTo(a * b))

    [<Test>]
    [<Repeat(50)>]
    let ``Given two numbers, m and n, if there is a common factor then modInverse returns none``() =
        let csprng = Csprng.create ()
        let makeNumbers = makeTuple <!> (Csprng.random 1) <*> (Csprng. random 1)
        let (a,b),_ = runState makeNumbers csprng

        Assert.That(Math.modularInverse a (a * b), Is.EqualTo(None))

    [<Test>]
    [<Repeat(30)>]
    let ``Given three numbers, a,b,c then lcm (a.b, a.c) = a . lcm(b,c)``() =
        let makeTriple a b c = a,b,c
        let csprng = Csprng.create ()
        let makeNumbers = makeTriple <!> (Csprng.random 1) <*> (Csprng. random 1) <*> (Csprng. random 1)
        let (a,b,c),_ = runState makeNumbers csprng

        Assert.That(lcm (a*b) (a*c), Is.EqualTo(a * (lcm b c)))