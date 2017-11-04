namespace RsaKeyDerivation.Test

module StateTests =
    open NUnit.Framework
    open RsaKeyDerivation
    open RsaKeyDerivation.State

    let simpleIncrementer : State<int, int> =
        State <| fun s -> (s, s+1)

    [<Test>]
    let ``When trying to replicate state zero times then the result is an empty list``() =
        let listState = State.replicateState 0 simpleIncrementer
        let resultList,resultState = runState listState 0
        Assert.That(resultList, Is.Empty)
        Assert.That(resultState, Is.Zero)

    [<Test>]
    let ``When trying to replicate state a negative number of times then the result is an empty list``() =
        let listState = State.replicateState -5 simpleIncrementer
        let resultList,resultState = runState listState 0
        Assert.That(resultList, Is.Empty)
        Assert.That(resultState, Is.Zero)

    [<Test>]
    let ``When replicating state then result is correct``() =
        let listState = State.replicateState 5 simpleIncrementer
        let resultList,resultState = runState listState 0
        Assert.That(resultList, Is.EqualTo([0;1;2;3;4]))
        Assert.That(resultState, Is.EqualTo(5))

    [<Test>]
    let ``Mapping a function over a state works correctly``() =
        let incrementerToString = (fun s -> s.ToString()) <!> simpleIncrementer
        let listStringState = State.replicateState 3 incrementerToString
        let resultList, resultState = runState listStringState 0
        Assert.That(resultList, Is.EqualTo(["0"; "1"; "2"]))
        Assert.That(resultState, Is.EqualTo(3))

    [<Test>]
    let ``Applying a function over a state values works``() =
        let createTuple a b = a,b
        let tupleState = createTuple <!> simpleIncrementer <*> simpleIncrementer
        let tuple,finalState = runState tupleState 0
        Assert.That(tuple, Is.EqualTo(0,1))
        Assert.That(finalState, Is.EqualTo(2))
