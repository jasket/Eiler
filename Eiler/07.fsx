// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
// What is the 10 001st prime number?

let isPrime x n = x % n = 0
let increment f n = if f then n+1 else n

let getPrimeCount n = 
  let rec getPrimeCount' from curr count = 
    match count with
    | k when k=n+1 -> from-1
    | _ -> 
        if (curr = from) || (isPrime from curr) then
          let count' = increment (curr=from) count
          getPrimeCount' (from+1) 2 count' 
        else getPrimeCount' from (curr+1) count
  getPrimeCount' 2 2 1

List.map (fun x -> getPrimeCount x) [1..6]
getPrimeCount 10001 //val it : int = 104743