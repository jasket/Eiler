//2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
//What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

let isPrime = fun (x:int64, n:int64) -> x % n = 0L
let product = List.fold (*) 1L
let multiply (min:int64, max:int64) = [min .. max] |> product

let isAllDivide (n:int64, max:int64) = 
    let rec isAllDivide' (n:int64, max:int64) = 
        match max with
        | 2L -> true
        | _ -> if isPrime (n, max) then isAllDivide' (n, max-1L) else false
    (n, max) |> isAllDivide'

let getDel (min, max) =
  seq {
    for n in max .. multiply (min, max) do
        if isAllDivide (n, max)
            then yield n
  }

getDel (2L, 20L) |> Seq.take 1 //232792560