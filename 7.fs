#load "util.fs"
open util

let t = get_prime_sequence ()
let p = Seq.nth 10000 t
