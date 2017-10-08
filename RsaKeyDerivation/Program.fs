namespace RsaKeyDerivation

module Main =
    open State

    let testMethod : State<int, int> =
        State <| fun s -> (s, s+1)

    let trio = replicateState 3 testMethod

    [<EntryPoint>]
    let main argv = 
        let rng = Csprng.create ()
        let primeFinder = Prime.findPrime 60 (bigint 65536)
        let (p, newState) = runState primeFinder rng

        printfn "%A" <| p
        0
