namespace RsaKeyDerivation

type State<'s, 'a> = State of ('s -> ('a * 's))

module State =
    let runState (State f) = f

    let liftPure x = State <| fun s -> (x,s)

    let map (f : 'a -> 'b) (State sf) : State<'s, 'b> =
        State <| (fun s ->
            let (a, newS) = sf s
            (f a, newS))
    let (<!>) = map

    let apply (stateF : State<'s, 'a -> 'b>) (stateA : State<'s, 'a>) : State<'s, 'b> =
        State <| (fun s ->
            let (f, midS) = runState stateF s
            let (a, finS) = runState stateA midS
            (f a, finS))
    let (<*>) = apply

    let bind (stateA : State<'s, 'a>) (f : 'a -> State<'s, 'b>) : State<'s, 'b> =
        State <| (fun s ->
            let (a, midS) = runState stateA s
            let (State sb) = f a
            sb midS)
    let (>>=) = bind

    type StateBuilder () =
        member this.Return(x) = liftPure x
        member this.ReturnFrom(x) = x
        member this.Bind(a, f) = a >>= f

    let state = StateBuilder()

    let private append (b : 'a) (bs : 'a list) : 'a list =
        b :: bs

    let replicateState (count : int) (sa : State<'s, 'a>) : State<'s, 'a list> =
        let rec loop count sa (acc : State<'s, 'a list>) =
            match count with
            | 0 -> acc
            | _ -> loop (count - 1) sa (append <!> sa <*> acc)
        loop count sa (liftPure [])