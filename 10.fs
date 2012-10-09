#load "util.fs"
open util

let answer =
    get_prime_sequence 2I
    |> Seq.takeWhile (fun e -> e < 2000000I)
    |> Seq.sum
