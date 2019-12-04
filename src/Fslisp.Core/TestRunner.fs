module Fslisp.Core.TestRunner

open System.IO

exception CommandFailedException of msg:string

type Command =
    | ParseSuccess of input:string * output:string
    | ParseFailure of input:string
    | CompileSuccess of input:string * output:string
    | CompileFailure of input:string
    | EvalSuccess of input:string * output:string
    | EvalFailure of input:string
    | EvalAll of input:string

[<RequireQualifiedAccess>]
module Command =
    let (>>) result f =
        match result with
        | Ok a -> f a
        | Error e -> raise (CommandFailedException e)

    let inline failure result =
        match result with
        | Ok a -> raise (CommandFailedException (a.ToString()))
        | Error _ -> ()

    let inline success output result =
        match result with
        | Ok a ->
            if a.ToString().TrimEnd() <> output then
                raise (CommandFailedException (a.ToString()))
        | Error e ->
                raise (CommandFailedException e)

    let parse = Parser.parseToEnd Parser.sexp "test"

    let execute (ctx: Context) (command: Command) =
        match command with
        | ParseSuccess (input, output) ->
            parse input |> success output
        | ParseFailure input ->
            parse input |> failure
        | CompileSuccess (input, output) ->
            parse input >> ctx.Compile |> success output
        | CompileFailure input ->
            parse input >> ctx.Compile |> failure
        | EvalSuccess (input, output) ->
            parse input >> ctx.Eval |> success output
        | EvalFailure input ->
            parse input >> ctx.Eval |> failure
        | EvalAll input ->
            match Parser.parseToEnd Parser.program "test" input with
            | Ok program ->
                for expr in program do
                    match ctx.Eval expr with
                    | Ok _ -> ()
                    | Error e -> raise (CommandFailedException e)
            | Error e ->
                raise (CommandFailedException e)

type TestCase =
    { Header: string; Command: Command }

[<RequireQualifiedAccess>]
module TestCase =
    let parse (src: Stream): TestCase list =
        use reader = new StreamReader(src)

        let readLines len =
            seq { for i in 1..len -> reader.ReadLine() }
            |> String.concat "\n"

        let readCommand header =
            match reader.ReadLine().Split(' ') with
            | [| "PARSE_SUCCESS"; input; output |] ->
                let input = readLines (int input)
                let output = readLines (int output)
                ParseSuccess (input, output)
            | [| "PARSE_FAILURE"; input |] ->
                let input = readLines (int input)
                ParseFailure input
            | [| "COMPILE_SUCCESS"; input; output |] ->
                let input = readLines (int input)
                let output = readLines (int output)
                CompileSuccess (input, output)
            | [| "COMPILE_FAILURE"; input |] ->
                let input = readLines (int input)
                CompileFailure input
            | [| "EVAL_SUCCESS"; input; output |] ->
                let input = readLines (int input)
                let output = readLines (int output)
                EvalSuccess (input, output)
            | [| "EVAL_FAILURE"; input |] ->
                let input = readLines (int input)
                EvalFailure input
            | [| "EVAL_ALL"; input |] ->
                let input = readLines (int input)
                EvalAll input
            | _ ->
                failwithf "Unknown test command: %s" header

        [
            while not reader.EndOfStream do
                let header = reader.ReadLine()
                let command = readCommand header
                yield { Header = header; Command = command }
        ]

    let run (ctx: Context) { Header = header; Command = command }: bool =
        try
            Command.execute ctx command
            false
        with
        | CommandFailedException msg ->
            eprintfn "Test failed at %s: %s" header msg
            true

let run (ctx: Context) (src: Stream) =
    src
    |> TestCase.parse
    |> Seq.filter (fun testCase -> testCase.Header.Contains "testsuites") // FIXME: temporary disabled
    |> Seq.map (TestCase.run ctx)
    |> Seq.filter id
    |> Seq.length
