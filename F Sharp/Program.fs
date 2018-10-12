// Learn more about F# at http://fsharp.org

open System

let rec fact = function
| 0 -> 1
| n -> n * fact (n-1)

[<EntryPoint>]
let main argv =
    printfn "fact 6 = %i" (fact 6)
    let pause = System.Console.ReadLine()
    0 // return an integer exit code