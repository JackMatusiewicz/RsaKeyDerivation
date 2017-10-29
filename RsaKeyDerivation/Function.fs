namespace RsaKeyDerivation

module Function =
    let curry (f : ('a *'b) -> 'c) (a : 'a) (b : 'b) = f (a, b)
    let uncurry (f : 'a -> 'b -> 'c) ((a,b) : 'a * 'b) = f a b

    let makeTuple (a : 'a) (b : 'b) = a,b

