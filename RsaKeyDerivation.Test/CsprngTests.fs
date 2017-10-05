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