namespace Fslisp.Core

type Context() =
    let topLevel = Env(None)
    do Syntax.install topLevel

    static member private Wrap f =
        try
            Ok(f())
        with
        | SyntaxErrorException e -> Error ("Syntax error: " + e)
        | CompileErrorException e -> Error ("Compile error: " + e)
        | EvaluationErrorException e -> Error ("Evaluation error: " + e)
        | InternalErrorException e -> Error ("Internal error: " + e)
        | UndefinedVariableException s -> Error ("Undefined variable: " + s)

    member __.Compile (expr: Value): Result<Code, string> =
        Context.Wrap (fun () ->
            Compiler.Compile topLevel expr
        )

    member __.MacroExpand (recurse: bool) (expr: Value): Result<Value, string> =
        Context.Wrap (fun () ->
            MacroExpander.MacroExpand topLevel recurse expr
        )

    member __.Eval (expr: Value): Result<Value, string> =
        Context.Wrap (fun () ->
            let expr = MacroExpander.MacroExpand topLevel true expr
            let code = Compiler.Compile topLevel expr
            VM.Execute topLevel code
        )