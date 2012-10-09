#load "util.fs"
open util

let nums = [3; 7; 8; 9; 11; 13; 16; 17; 19]

let isDivisible x =
    List.map (fun n -> x % n) nums
    |> List.exists (fun e -> e <> 0)
    |> not

let find_divisible =
    Seq.initInfinite (fun i -> 20 + (20 * i))
    |> Seq.find (fun a -> (isDivisible a) = true)
