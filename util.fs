module util

open System
open System.Numerics

module NumericLiteralG = 
    let inline FromZero() = LanguagePrimitives.GenericZero
    let inline FromOne() = LanguagePrimitives.GenericOne
    let inline FromInt32 (n:int) =
        let one : ^a = FromOne()
        let zero : ^a = FromZero()
        let n_incr = if n > 0 then 1 else -1
        let g_incr = if n > 0 then one else (zero - one)
        let rec loop i g = 
            if i = n then g
            else loop (i + n_incr) (g + g_incr)
        loop 0 zero 

let inline isqrt num =
    if num > 0G then
        let inline reduce n = (num / n + n) / 2G
        let rec impl n = function
            | n' when n' <= n -> n'
            | _ -> impl (reduce n) n
        let n = num / 2G + 1G
        impl (reduce n) n
    elif num = 0G then num
    else invalidArg "num" "Negative numbers are not supported"

let inline dsqrt x =
    match box x with
    | :? float as d -> sqrt d |> box :?> 'a
    | :? float32 as s -> sqrt s |> box :?> 'a
    | _ -> isqrt x

let inline isPrime (x:^a) =
    if x = 1G || x = 2G || x = 3G then true
    elif (x % 2G) = 0G then false
    else
        let limit = dsqrt(x) + 1G

        seq { 3G .. 2G .. limit }
        |> Seq.exists (fun (i:^a) -> (x % i) = 0G)
        |> not

let inline next_prime x =
    let rec comp x =
        match x with
        | n when x = 2G -> 3G
        | n when isPrime (x + 2G) = true -> x + 2G
        | _ -> comp (x + 2G)
    comp x

let inline get_prime_sequence start =
    start
    |> Seq.unfold (fun x -> Some(x, next_prime x))

let inline factors (x:^a) =
    let limit:^a = dsqrt(x)
    seq { for n in 2G .. limit do if (x % n) = 0G then yield n }

let inline factor_pairs (x:^a) =
    let limit:^a = dsqrt(x)
    [ for n in 1G .. limit do if (x % n) = 0G then yield ( n, x / n) ]

let get_proper_divisors x =
    factor_pairs x
    |> List.collect (fun b -> (fst b) :: [(snd b)])
    |> List.filter (fun a -> a <> x)
    |> List.sort

module Seq =
    let foralli f s =
        s
        |> Seq.mapi (fun i x -> i, x)
        |> Seq.forall (fun (i, x) -> f i x)

let inline isPalindrome input =
    let s = string(input)
    s
    |> Seq.take (s.Length / 2)
    |> Seq.foralli (fun i x -> x = s.[s.Length - i - 1])

let is_abundant n =
    List.sum (get_proper_divisors n) > n

let is_deficient n =
    List.sum (get_proper_divisors n) < n

let is_perfect n =
    List.sum (get_proper_divisors n) = n

let get_abundant_sequence () =
    let start = 12G
    Seq.initInfinite (fun x -> x + start)
    |> Seq.filter (fun x -> is_abundant x = true)

let get_deficient_sequence () =
    let start = 12G
    Seq.initInfinite (fun x -> x + start)
    |> Seq.filter (fun x -> is_deficient x = true)

let get_perfect_sequence () =
    let start = 12G
    Seq.initInfinite (fun x -> x + start)
    |> Seq.filter (fun x -> is_perfect x = true)

let inline dpow a b =
    let rec proc start c d =
        match d with
        | n when d = 1G -> c * start
        | _ -> proc start (c * start) (d - 1G)
    proc a 1G b

let inline next_fib term =
    let a, b = term
    Some(b, (b, a + b))

let get_fib_sequence =
    Seq.unfold next_fib (0, 1)

let inline fac n =
    let rec proc x =
        match x with
        | f when x = 0G -> 1G
        | _ -> x * proc (x - 1G)
    proc n
