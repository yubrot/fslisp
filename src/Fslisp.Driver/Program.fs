open System.IO
open Fslisp.Core

[<EntryPoint>]
let main argv =
    let ctx = Context(Builtins.table)
    match argv with
    | [| "-test"; test |] ->
        use fs = File.OpenRead(test)
        let fails = TestRunner.run ctx fs
        if fails = 0 then
            0
        else
            eprintfn "Total %d tests failed." fails
            1
    | ls ->
        0
