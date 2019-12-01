open System.IO
open Fslisp.Core

[<EntryPoint>]
let main argv =
    match Array.toList argv with
    | "-test" :: tests ->
        for test in tests do
            use fs = File.OpenRead(test)
            TestRunner.run fs
    | ls ->
        ()
    0
