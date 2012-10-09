#load "util.fs"
open util

let n = dpow 2I 1000I

let inline decompose a =
    let rec proc b =
        let f, s = b
        match f with
        | x when f < 10G -> (f, s + f)
        | _ -> proc (f / 10G, s + (f % 10G))
    snd(proc (a, 0G))

let answer = decompose n
