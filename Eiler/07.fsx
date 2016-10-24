// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
// What is the 10 001st prime number?

let isPrime (x, n) = x % n = 0

let getPrimeCount n = 
  let rec getPrimeCount' (from, curr, count) = 
    match count with
    | k when k=10 -> from
    | k when k=n -> from
    | _ ->        
        if ((curr = from) && isPrime (from, curr))
        then getPrimeCount' (from+1, 2, if (curr = from) then count+1 else count) 
        else getPrimeCount' (from, curr+1, count)
  getPrimeCount' (2, 2, 1)

getPrimeCount 6