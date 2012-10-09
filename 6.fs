#load "util.fs"
open util

let first_hundred_naturals = Seq.initInfinite (fun x -> x + 1) |> Seq.take 100
let sum_squares = Seq.sum (Seq.map (fun x -> x * x) first_hundred_naturals)

let s = Seq.sum first_hundred_naturals

let square_sum = s * s

let answer = abs(sum_squares - square_sum)
