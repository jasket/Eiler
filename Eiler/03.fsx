// The prime factors of 13195 are 5, 7, 13 and 29.
// What is the largest prime factor of the number 600851475143

let isPrime = fun (x:int64, n:int64) -> x % n = 0L
let isBigger = fun (x:int64, n:int64) -> if x > n then x else n

let getBiggestPrime n:int64 = 
    let rec allPrimes'(k:int64, i:int64, num:int64) =
        match k with
        | 1L -> num
        | _ -> if isPrime (k, i) then (k/i, 2L, (num, i) |> isBigger) |> allPrimes' else (k, i+1L, num) |> allPrimes'     
    (n, 2L, 0L) |> allPrimes'

getBiggestPrime 600851475143L //val it : int64 = 6857L