// A palindromic number reads the same both ways. The largest palindrome made from 
// the product of two 2-digit numbers is 9009 = 91 × 99.
// Find the largest palindrome made from the product of two 3-digit numbers.

let exp10 n = (float 10)**(float n)
let min n = exp10 (n-1) |> int
let max n = (exp10 n |> int) - 1
 
let reverse s:string = List.ofSeq s |> List.rev |> List.toArray |> System.String
let isPolindrom = fun n -> (string n = (string n |> reverse))

let find n = 
    seq {
      for i in min n .. max n do
          for j in min n .. max n do
            if isPolindrom (i*j) then yield i*j
    }

printfn "%A" (find 3 |> Seq.distinct |> Seq.sort |> Seq.rev |> Seq.take 1) // 906609

