﻿namespace RsaKeyDerivation

open System.Numerics

module Math =

    let lcm (a : BigInteger) (b : BigInteger) =
        if a = (bigint 0) && b = (bigint 0) then
            (bigint 0)
        else
            let gcd = BigInteger.GreatestCommonDivisor(a,b)
            (a * b) / gcd

    let rec extendedGcd (a : bigint) (b : bigint) =
        match a,b with
        | (a,b) when b = (bigint 0) -> (bigint 1, bigint 0, a)
        | _ ->
            let quotient = BigInteger.Divide(a,b)
            let rem = BigInteger.Remainder(a,b)
            let (x,y,z) = extendedGcd b rem
            (y, x - quotient * y, z)

    let modularInverse (a : bigint) (modulus : bigint) =
        let (i, _, k) = extendedGcd a modulus
        if k = (bigint 1) then
            if i < (bigint 0) then
                Some <| i + modulus
            else
                Some i
        else None

