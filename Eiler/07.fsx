// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
// What is the 10 001st prime number?

let isPrime = fun (x, n) -> x % n = 0
let rec numbersFrom n = 
  seq { yield n
        yield! numbersFrom (n + 1) }


numbersFrom 2 |> Seq.take 6