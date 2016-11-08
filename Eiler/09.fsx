//A Pythagorean triplet is a set of three natural numbers, a < b < c, for which, a^2 + b^2 = c^2
//For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
//There exists exactly one Pythagorean triplet for which a + b + c = 1000.
//Find the product abc.

let getZ x y n = n - x - y
let isTriplet x y n = x**2. + y**2. = (getZ x y n)**2.

let cicle n = seq {
    for x in [1.0..n-2.] do 
        for y in [x..n-2.] do
            if(isTriplet x y n) then
                let z = getZ x y n
                yield (x, y, z, x * y * z)
}

cicle 1000. //[(200.0, 375.0, 425.0, 31875000.0)]