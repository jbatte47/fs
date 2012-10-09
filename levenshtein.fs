let inline min3 one two three =
    if one < two && one < three then one
    elif two < three then two
    else three

let levenshtein (s : string) (t : string) =
    let m = s.Length
    let n = t.Length
    let d = Array2D.create (m + 1) (n + 1) 0

    for i = 0 to m do d.[i, 0] <- i
    for j = 0 to n do d.[0, j] <- j

    for j = 1 to n do
        for i = 1 to m do
            (*printfn "comparing %c to %c" s.[i - 1] t.[j - 1]*)
            if s.[i - 1] = t.[j - 1] then
                d.[i, j] <- d.[i - 1, j - 1]
            else
                d.[i, j] <-
                      min3
                        (d.[i - 1, j] + 1)
                        (d.[i, j - 1] + 1)
                        (d.[i - 1, j - 1] + 1)
    d.[m, n]

let levenshtein_lazy (s : string) (t : string) =
    let m = s.Length
    let n = t.Length
    let d = Array2D.create (m + 1) (n + 1) -1

    let rec dist =
        function
        | i, 0 -> i
        | 0, j -> j
        | i, j when d.[i, j] <> -1 -> d.[i, j]
        | i, j ->
            let dval =
                if s.[i - 1] = t.[j - 1] then
                    dist (i - 1, j - 1)
                else
                    min3
                        (dist (i - 1, j) + 1)
                        (dist (i, j - 1) + 1)
                        (dist (i - 1, j - 1) + 1)
            d.[i, j] <- dval; dval
    dist (m, n)
