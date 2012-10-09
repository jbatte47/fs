#load "util.fs"
open util

let rec inner b a =
    let c = dsqrt(float((a * a) + (b * b)))
    let p = float(a + b) + c

    match p with
    | 1000.0 -> a * b * (int c)
    | _ when a >= 1000 -> -1
    | _ -> inner b (a + 1)

let rec outer b =
    let prod = inner b 2
    match prod with
    | -1 when b >= 1000 -> 0
    | -1 -> outer (b + 1)
    | _ -> prod

let answer = outer 2
