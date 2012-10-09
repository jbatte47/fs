let multiples = List.filter (fun x -> x % 3 = 0 || x % 5 = 0) [1 .. 999]
let s = List.sum multiples
printf "%i" s;;
