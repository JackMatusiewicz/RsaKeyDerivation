namespace RsaKeyDerivation

module Main =
    open State

    let testMethod : State<int, int> =
        State <| fun s -> (s, s+1)

    let trio = replicateState 3 testMethod

    [<EntryPoint>]
    let main argv = 
        printfn "%A" <| runState trio 1
        0 // return an integer exit code
