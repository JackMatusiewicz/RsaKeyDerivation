namespace RsaKeyDerivation.Test

module PrimeTests =
    open NUnit.Framework
    open RsaKeyDerivation
    open RsaKeyDerivation.State

    [<Test>]
    let ``Given start numbers, when trying to find next prime then correct number is chosen`` () =
        let startToPrime = [
            (bigint 16, bigint 17)
            (bigint 65536, bigint 65537)
            (bigint 7), (bigint 7)
        ]
        let csprng = Csprng.create ()
        List.iter (fun (n, truePrime) ->
                    let p, _ = runState (Prime.findPrime 60 n) csprng
                    Assert.That(p, Is.EqualTo(truePrime))) startToPrime