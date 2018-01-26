namespace RsaKeyDerivation

type State<'s, 'a> = State of ('s -> ('a * 's))

module State =
    let runState (State f) = f

    let lift x = State <| fun s -> (x,s)

    let map (f : 'a -> 'b) (State sf) : State<'s, 'b> =
        State <|
            fun s ->
                let (a, newS) = sf s
                (f a, newS)
    let (<!>) = map
    let (<?>) (a : State<'s, 'a>) (f : 'a -> 'b) : State<'s, 'b> =
        map f a

    let apply (stateF : State<'s, 'a -> 'b>) (stateA : State<'s, 'a>) : State<'s, 'b> =
        State <|
            fun s ->
                let (f, midS) = runState stateF s
                let (a, finS) = runState stateA midS
                (f a, finS)
    let (<*>) = apply

    let bind (stateA : State<'s, 'a>) (f : 'a -> State<'s, 'b>) : State<'s, 'b> =
        State <|
            fun s ->
                let (a, midS) = runState stateA s
                let (State sb) = f a
                sb midS
    let (>>=) = bind
    
    let compose (f : 'a -> State<'s, 'b>) (g : 'b -> State<'s, 'c>) : 'a -> State<'s, 'c> =
        fun a -> f a >>= g
    let (>=>) = compose

    type StateBuilder () =
        member this.Return(x) = lift x
        member this.ReturnFrom(x) = x
        member this.Bind(a, f) = a >>= f
    let state = StateBuilder()

    let private append (b : 'a) (bs : 'a list) : 'a list =
        b :: bs

    let replicateState (count : int) (sa : State<'s, 'a>) : State<'s, 'a list> =
        let rec loop count sa (acc : State<'s, 'a list>) =
            match count with
            | _ when count <= 0 -> acc
            | _ -> loop (count - 1) sa (append <!> sa <*> acc)
        loop count sa (lift [])

    let appendSeq (a : 'a) (acc : 'a seq) = seq {
        yield a
        yield! acc
    }

    let replicateStateSeq (count  : int) (sa : State<'s, 'a>) : State<'s, 'a seq> =
        let rec loop sa (acc : State<'s, 'a seq>) count =
            match count with
            | 0 -> acc
            | _ -> loop sa (appendSeq <!> sa <*> acc) (count - 1)
        loop sa (lift <| Seq.empty) count
