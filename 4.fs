#load "util.fs"
open util

let rec inner o i m =
    let p = o * i
    match p with
    | p when isPalindrome p && i <= 2 -> max m p
    | p when isPalindrome p -> inner o (i - 1) (max m p)
    | _ -> inner o (i - 1) m

let rec outer o m =
    let f = inner o 999 m
    match f with
    | _ when o <= 2 -> max f m
    | _ -> outer (o - 1) (max f m)
