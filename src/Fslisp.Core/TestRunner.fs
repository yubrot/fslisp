namespace Fslisp.Core

open System.IO

module TestRunner =
    exception CommandFailedException of msg:string

    type Command =
        | ParseSuccess of input:string * result:string
        | ParseFailure of input:string
        | CompileSuccess of input:string * result:string
        | CompileFailure of input:string
        | EvalSuccess of input:string * result:string
        | EvalFailure of input:string
        | EvalAll of input:string

    module Command =
        let private fail msg =
            raise (CommandFailedException msg)

        let private failIfDiffer (actual: string) (expected: string) =
            if actual.Trim() <> expected.Trim() then
                raise (CommandFailedException actual)

        let execute command =
            match command with
            | ParseSuccess (input, result) ->
                match Parser.parseToEnd Parser.sexp "test" input with
                | Error e -> fail e
                | Ok s -> failIfDiffer (Sexp.prettyPrint id s) result
            | ParseFailure input ->
                match Parser.parseToEnd Parser.sexp "test" input with
                | Error _ -> ()
                | Ok s -> fail (Sexp.prettyPrint id s)
            | CompileSuccess (input, result) ->
                ()
            | CompileFailure input ->
                ()
            | EvalSuccess (input, result) ->
                ()
            | EvalFailure input ->
                ()
            | EvalAll input ->
                ()

    type TestCase =
        { Header: string; Command: Command }

    let parseTestCases (src: Stream): TestCase list =
        use reader = new StreamReader(src)

        let readLines len =
            seq { for i in 1..len -> reader.ReadLine() }
            |> String.concat "\n"

        let readCommand header =
            match reader.ReadLine().Split(' ') with
            | [| "PARSE_SUCCESS"; input; result |] ->
                let input = readLines (int input)
                let result = readLines (int result)
                ParseSuccess (input, result)
            | [| "PARSE_FAILURE"; input |] ->
                let input = readLines (int input)
                ParseFailure input
            | [| "COMPILE_SUCCESS"; input; result |] ->
                let input = readLines (int input)
                let result = readLines (int result)
                CompileSuccess (input, result)
            | [| "COMPILE_FAILURE"; input |] ->
                let input = readLines (int input)
                CompileFailure input
            | [| "EVAL_SUCCESS"; input; result |] ->
                let input = readLines (int input)
                let result = readLines (int result)
                EvalSuccess (input, result)
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

    let runTestCase { Header = header; Command = command } =
        try
            Command.execute command
        with
        | CommandFailedException msg ->
            eprintfn "Test failed at %s: %s" header msg

    let run src =
        src
        |> parseTestCases
        |> List.iter runTestCase
