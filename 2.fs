open System.Numerics

let limit = BigInteger(4000000 - 1)
let fib =
    Seq.unfold
        (fun (current, next) -> Some(current, (next, current + next)))
        (BigInteger 0, BigInteger 1)

let even_fibs_sum =
    fib
    |> Seq.takeWhile (fun n -> n < limit)
    |> Seq.filter (fun n -> n % bigint(2) = bigint(0))
    |> Seq.sum
