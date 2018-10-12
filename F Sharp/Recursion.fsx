let rec fact = function
|0 -> 1
|n -> n * fact(n-1)

fact (5)

let rec min = function
| [] -> []
| [x] -> [x]
| x1::x2::xs -> if x1 < x2 then (min (x1::xs) @ [x2])
                else (min (x2::xs) @ [x1])

min ([7;4;9;8;0;4;3;6;1;3;6])

let rec selectionSort = function
| [] -> []
| [x] -> [x]
| xs -> let min1::mins = min (xs)
        min1 :: (selectionSort (mins))

selectionSort ([7;4;9;8;0;4;3;6;1;3;6])
        
