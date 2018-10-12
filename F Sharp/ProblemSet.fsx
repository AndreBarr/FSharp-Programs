open System.Diagnostics

type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF

let eat token = function
| [] -> failwith "Error"
| x::xs -> if x = token then xs
           else failwith "Error"

let rec S = function
| [] -> failwith "Error"
| x::xs -> match x with
           | IF -> xs |> E |> eat THEN |> S |> eat ELSE |> S
           | BEGIN -> xs |> S |> L
           | PRINT -> xs |> E
           | _ -> failwith "Error"

and L = function
| [] -> failwith "Error"
| x::xs -> match x with
           | END -> xs
           | SEMICOLON -> xs |> S |> L
           | _ -> failwith "Error"

and E = function
| [] -> failwith "Error"
| x::xs -> match x with
           | ID -> xs
           | _ -> failwith "Error"

let accept = "Valid"

let error = "Not Valid"

let test_program program =
    let result = program |> S
    match result with 
    | [] -> failwith "Early termination or missing EOF"
    | x::xs -> if x = EOF then accept else error

test_program [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]

let curry f = fun x -> fun y -> f (x,y)

let add (a,b) = a + b

let newadd = curry add

let test = newadd 2

test 3

let uncurry f = fun (x, y) -> f x y

let plus = uncurry (+)

plus (2,3)

type TERMINAL2 = PLUS|MINUS|TIMES|DIVIDE|ID|EOF|OPENP|CLOSEP

let rec E2 = function
| [] -> failwith "Error"
| x::xs -> let y::ys = T2(x::xs)
           match y with
           | PLUS -> E2 ys
           | MINUS -> E2 ys
           | _ -> y::ys

and T2 = function
| [] -> failwith "Error"
| x::xs -> let y::ys = F2(x::xs)
           match y with
           | TIMES -> T2 ys
           | DIVIDE -> T2 ys
           | _ -> y::ys

and F2 = function
| [] -> failwith "Error"
| x::xs -> match x with
           | ID -> xs
           | OPENP -> xs |> E2 |> F2
           | CLOSEP -> xs

let test_arithmetic program =
    let result = program |> E2
    match result with 
    | [] -> failwith "Early termination or missing EOF"
    | x::xs -> if x = EOF then "Accept" else "Error"

test_arithmetic [ID;PLUS;OPENP;ID;TIMES;ID;CLOSEP;EOF]