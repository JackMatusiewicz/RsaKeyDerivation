namespace RsaKeyDerivation.Test

module CsprngTests =
    open NUnit.Framework
    open RsaKeyDerivation
    open RsaKeyDerivation.State
    open System.Linq

    [<Test>]
    let ``Given a csprng when same key used twice then same block is generated`` () =
        let csprng = Csprng.create ()
        let (block, _) = runState Csprng.generateBlock csprng
        let (blockTwo, _) = runState Csprng.generateBlock csprng
        let data = Csprng.getData block
        let dataTwo = Csprng.getData blockTwo
        Assert.That(data.SequenceEqual(dataTwo), Is.EqualTo(true))

    [<Test>]
    let ``Given a csprng when same key used twice then same number is generated`` () =
        let csprng = Csprng.create ()
        let (num, _) = runState (Csprng.generate 4) csprng
        let (numTwo, _) = runState (Csprng.generate 4) csprng
        Assert.That(num, Is.EqualTo(numTwo))

    [<Test>]
    let ``Given a csprng when a number is generated then counter is incremented correctly`` () =
        let csprng = Csprng.create ()
        let (num, cp) = runState (Csprng.generate 4) csprng
        Assert.That(cp.Counter, Is.EqualTo((bigint 4)))

    [<Test>]
    let ``Given csprng when used to generate many numbers then counter is incremented correctl`` () =
        let csprng = Csprng.create ()
        let randomNumbersGenerator = replicateState 10 (Csprng.generate 4)
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

    [<Test>]
    let ``Given a csprng, when tested using both generate methods then result is the same`` () =
        let rec test (csprng : Csprng) (count : int) =
            match count with
            | 0 -> ()
            | _ ->
                let (n1, updatedCsprng) = runState (Csprng.generate 4) csprng
                let (n2, updatedCsprng2) = runState (Csprng.generateRandom 4) csprng
                Assert.That(n1, Is.EqualTo(n2))
                Assert.That(updatedCsprng.Counter, Is.EqualTo(updatedCsprng2.Counter))
                test updatedCsprng (count - 1)
        let csprng = Csprng.create ()
        test csprng 1000