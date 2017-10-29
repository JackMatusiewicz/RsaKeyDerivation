namespace RsaKeyDerivation.Test

module CsprngTests =
    open NUnit.Framework
    open RsaKeyDerivation
    open RsaKeyDerivation.State

    [<Test>]
    let ``Given a csprng when same key used twice then same block is generated`` () =
        let csprng = Csprng.create ()
        let (num, _) = runState (Csprng.random 1) csprng
        let (numTwo, _) = runState (Csprng.random 1) csprng
        Assert.That(num, Is.EqualTo(numTwo))

    [<Test>]
    let ``Given a csprng when same key used twice then same number is generated`` () =
        let csprng = Csprng.create ()
        let (num, _) = runState (Csprng.random 4) csprng
        let (numTwo, _) = runState (Csprng.random 4) csprng
        Assert.That(num, Is.EqualTo(numTwo))

    [<Test>]
    let ``Given a csprng when a number is generated then counter is incremented correctly`` () =
        let csprng = Csprng.create ()
        let (num, cp) = runState (Csprng.random 4) csprng
        Assert.That(cp.Counter, Is.EqualTo((bigint 4)))

    [<Test>]
    let ``Given csprng when used to generate many numbers then counter is incremented correctl`` () =
        let csprng = Csprng.create ()
        let randomNumbersGenerator = replicateState 10 (Csprng.random 4)
        let (_, updatedCsprng) = runState randomNumbersGenerator csprng
        Assert.That(updatedCsprng.Counter, Is.EqualTo((bigint 40)))

    //Not a great test since it isn't really conclusive, just here to ensure nothing is blatantly wrong as I dev.
    [<Test>]
    let ``Given a csprng when a number is generated in a range then the number is always in the range`` () =
        let csprng = Csprng.create ()
        let min = bigint 5
        let max = bigint 37
        let randomNumbers = replicateState 100 (Csprng.range min max)
        let (numbers, cp) = runState randomNumbers csprng
        List.iter (fun (n : bigint) ->
                    Assert.That(n, Is.GreaterThanOrEqualTo(min))
                    Assert.That(n, Is.LessThan(max))) numbers

    //Property based testing would be good here.
    [<Test>]
    let ``Given number with msb not set, when attempt to set msb then number is correct`` () =
        let value = bigint 8
        let newValue = Csprng.setMsb value
        Assert.That(newValue, Is.EqualTo(bigint 136))