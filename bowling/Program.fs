// Learn more about F# at http://fsharp.org

open System
open Bowling

[<EntryPoint>]
let main argv =
    printfn "%A" (parse "X 5 5 X 6 - X X X - - - - X 5 5")
    0 // return an integer exit code
