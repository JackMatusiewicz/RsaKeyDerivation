namespace RsaKeyDerivation.Test

open NUnit.Framework
open RsaKeyDerivation
open RsaKeyDerivation.State

module PrimeTests =

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
                    Assert.That(Prime.toBigInt p, Is.EqualTo(truePrime))) startToPrime

    [<Test>]
    let ``Given non-prime number, trying to construct a prime results in nothing`` () =
        let csprng = Csprng.create ()
        let x,_ = runState (Prime.createPrime 4 (bigint 65536)) csprng
        Assert.That(x, Is.EqualTo(None))

    [<Test>]
    let ``Given prime number, trying to construct a prime results in a prime`` () =
        let csprng = Csprng.create ()
        let x,_ = runState (Prime.createPrime 4 (bigint 11)) csprng
        match x with
        | None -> Assert.Fail("This should be a valid prime")
        | Some p -> p |> Prime.toBigInt |> (fun p -> Assert.That(p, Is.EqualTo((bigint 11))))