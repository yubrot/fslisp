open System
open System.IO
open Fslisp.Core

let ignoreOk result =
    match result with
    | Ok _ -> ()
    | Error e ->
        eprintfn "%s" e
        exit 1

let parseAndEval (ctx: IContext) name input =
    let rec evalAll index result program =
        match program with
        | expr :: rest ->
            match ctx.Eval expr with
            | Ok result -> evalAll (index + 1) result rest
            | Error e -> Error(sprintf "%s[%d]: %s" name index e)
        | [] -> Ok result

    Parser.parseToEnd Parser.program name input |> Result.bind (evalAll 1 Sexp.Nil)

let initContext (args: string list) (boot: bool) =
    let builtins = BuiltinRegistry()
    Std.install args builtins
    let ctx = Context(builtins)

    if boot then
        let bootCodePath = Path.Combine(AppContext.BaseDirectory, "boot.lisp")
        let bootCode = File.ReadAllText bootCodePath
        parseAndEval ctx "init" bootCode |> ignoreOk

    ctx

let runREPL (ctx: IContext) =
    eprintfn "[fslisp REPL]"

    while true do
        eprintf "> "
        Console.Error.Flush()
        let line = Console.In.ReadLine()

        match parseAndEval ctx "<stdin>" line with
        | Ok v -> printfn "%s" (v.ToString())
        | Error e -> printfn "%s" e

[<EntryPoint>]
let main argv =
    match argv with
    | [||] ->
        initContext [] true |> runREPL
        0
    | [| "-test"; test |] ->
        let ctx = initContext [] false
        use fs = File.OpenRead(test)
        let fails = TestRunner.run ctx fs

        if fails = 0 then
            0
        else
            eprintfn "Total %d tests failed." fails
            1
    | argv ->
        let sepIndex = Array.tryFindIndex (fun s -> s = "--") argv

        let inputFiles, lispArgs =
            match sepIndex with
            | Some index -> Seq.take index argv, Seq.skip (index + 1) argv
            | None -> Seq.ofArray argv, Seq.empty

        let ctx = initContext (List.ofSeq lispArgs) true

        for inputFile in inputFiles do
            let input = File.ReadAllText inputFile
            parseAndEval ctx inputFile input |> ignoreOk

        0
