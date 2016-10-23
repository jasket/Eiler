// The sum of the squares of the first ten natural numbers is,
// 1^2 + 2^2 + ... + 10^2 = 385
// The square of the sum of the first ten natural numbers is,
// (1 + 2 + ... + 10)^2 = 55^2 = 3025
// Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
// Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

let squares x = seq { for i in 0. .. x do yield i**2. }
let square x = x ** 2.

let f1 x = squares x |> Seq.sum |> float
let f2 x = [1. .. x] |> List.sum |> square
let div x = f2 x - f1 x

div 100.


