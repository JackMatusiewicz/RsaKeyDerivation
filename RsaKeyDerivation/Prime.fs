namespace RsaKeyDerivation

type Prime = private Prime of bigint

module Prime =
    open System.Numerics
    open State

    let toBigInt (Prime p) = p

    type WitnessCheckResult = Composite | Continue

    let private calculateComposite (n : bigint) : (int * bigint) =
        let rec calc ((s,d) : int * bigint) : (int * bigint) =
            if d.IsEven then
                calc (s + 1, d / (bigint 2))
            else
                (s,d)
        calc (0, n)

    let millerRabinTest (k : int) (n : bigint) : State<Csprng, bool> =
        let rec witnessCheck (x : bigint) (n : bigint) (r : int) : WitnessCheckResult =
            match r with
            | 0 -> Composite
            | _ ->
                let newX = BigInteger.ModPow(x, bigint 2, n)
                if newX = (bigint 1) then
                    Composite
                else if newX = (n - bigint 1) then
                    Continue
                else
                    witnessCheck newX n (r - 1)

        let rec attempts (k : int) : State<Csprng, bool> = state {
            match k with
            | 0 -> return true
            | _ ->
                let (s,d) = calculateComposite (n - bigint 1)
                let! a = Csprng.range (bigint 2) (n - bigint 2)
                let x = BigInteger.ModPow(a, d, n)
                if (x = bigint 1 || x = (n - bigint 1)) then
                    return! attempts (k - 1)
                else
                    match witnessCheck x n (s - 1) with
                    | Composite -> return false
                    | Continue -> return! attempts (k - 1)
        }
        attempts k

    let isPrime (checks : int) : bigint -> State<Csprng, bool> =
        let remainderIsZero num denom =
            BigInteger.Remainder(num, denom) = (bigint 0)

        let smallPrimes = [2;3;5;7;11] |> List.map bigint
        let smallPrimeTest = fun v -> 
            let isMultipleOfPrimes = List.map (remainderIsZero v) smallPrimes
            not <| List.fold (||) false isMultipleOfPrimes

        fun n ->
            if List.contains n smallPrimes then
                lift true
            else
                (&&) <!> (lift <| smallPrimeTest n) <*> (millerRabinTest checks n)

    let findPrime (k : int) (start : bigint) : State<Csprng, Prime> =
        let rec findNextPrime (current : bigint) : State<Csprng, Prime> = state {
            let! valueIsPrime = isPrime k current
            match valueIsPrime with
            | true -> return current |> Prime
            | false -> return! findNextPrime (current + bigint 2)
        }
        if start.IsEven then
            findNextPrime (start - bigint 1)
        else
            findNextPrime start
