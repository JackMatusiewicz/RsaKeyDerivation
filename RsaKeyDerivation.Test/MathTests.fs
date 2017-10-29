namespace RsaKeyDerivation.Test

module MathTests =
    open NUnit.Framework
    open RsaKeyDerivation
    open RsaKeyDerivation.State

    [<Test>]
    let ``Given random modulus and number, when calculating modular inverse then result is correct``() =
        let mutable csprng = Csprng.create ()
        for i in 1 .. 1000 do
            let modulus,newCsprng = runState (Csprng.randomForRsa 4) csprng
            let a,finalCsprng = runState (Csprng.randomForRsa 4) newCsprng
            csprng <- finalCsprng
            let modInv = Math.modInverse a modulus
            Assert.That((a * modInv) % modulus, Is.EqualTo(bigint 1))