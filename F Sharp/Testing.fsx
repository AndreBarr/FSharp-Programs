let cos_squared r =
    let c = cos r
    c*c

cos_squared 10.0

printfn "cos_squared of 10.0 is: %f" (cos_squared 10.0)

let mk_expon times one =
    let rec expon x n =
        if n = 0 then one
        elif n%2 = 0 then expon (times x x) (n/2)
        else times x (expon x (n-1))
    expon

let expon1 = mk_expon (*) 1

expon1 2 4

let root (a,b,c) = 
    let disc = sqrt (b*b - 4.0*a*c)
    let twoa = 2.0*a
    ((-b + disc)/twoa,((-b - disc)/twoa))

root (2.0, 8.0, 4.0)