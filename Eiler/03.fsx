// The prime factors of 13195 are 5, 7, 13 and 29.
// What is the largest prime factor of the number 600851475143

let isPrime = fun (n, x) -> x % n = 0

let allPrimes num =
  let rec allPrimes' (n, x) =
    seq { // sequences are lazy, so we can make them infinite
      if isPrime (n, x) then
        yield n
      yield! allPrimes' ((n+1), x) // recursing
    }
  allPrimes'(2, num) // starting from 2

allPrimes 13195
|> Seq.take 10 // only 20
|> List.ofSeq // forces evaluation of first 20 items