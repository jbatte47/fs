#load "util.fs"
open util

let inline collatz n =
    match n with
    | x when n = 0G -> None
    | x when n = 1G -> Some(1G, 0G)
    | x when (n % 2G) = 0G -> Some(x, x / 2G)
    | _ -> Some(n, (3G * n) + 1G)

let inline get_collatz_sequence start =
    Seq.unfold collatz start

let inline coll_maxer (n:^a) (m:^a) max_length total =
    let rec proc (n:^a) (m:^a) max_length total =
        let len = Seq.length (get_collatz_sequence (int64 n))
        let a =
            if len >= max_length then
                (n, len)
            else
                total
        match n with
        | x when n < m -> proc (n + 1G) m (max len max_length) a
        | _ -> a
    proc n m max_length (n, 0)

let m = coll_maxer 3 1000000 0 0
