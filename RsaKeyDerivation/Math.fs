namespace RsaKeyDerivation

module Math =
    open System.Numerics

    let lcm (a : BigInteger) (b : BigInteger) =
        if a = (bigint 0) && b = (bigint 0) then
            (bigint 0)
        else
            let gcd = BigInteger.GreatestCommonDivisor(a,b)
            (a * b) / gcd

    let modInverse (a : bigint) (modValue : bigint) =
        let mutable x = (bigint 0)
        let mutable y = (bigint 1)
        let mutable u = (bigint 1)
        let mutable v = (bigint 0)
        let mutable e = modValue
        let mutable f = a
        let mutable c = (bigint 0)
        let mutable d = (bigint 0)
        let mutable q = (bigint 0)
        let mutable r = (bigint 0)
        while (f <> (bigint 1)) do
            q <- e / f
            r <- e % f
            c <- x - q * u
            d <- y - q * v
            x <- u
            y <- v
            u <- c
            v <- d
            e <- f
            f <- r
        (u + modValue) % modValue

