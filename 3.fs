#load "util.fs"
open util

let v = 600851475143I

let s = Seq.max (seq { for i in factors v do if isPrime i then yield i })
