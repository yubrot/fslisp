namespace Fslisp.Core

type Context() =
    let topLevel = Env(None)
    do Syntax.install topLevel

    member self.Try f =
        try
            Ok(f())
        with
        | SyntaxErrorException e -> Error ("Syntax error: " + e)
        | CompileErrorException e -> Error ("Compile error: " + e)
        | EvaluationErrorException e -> Error ("Evaluation error: " + e)
        | InternalErrorException e -> Error ("Internal error: " + e)
        | UndefinedVariableException s -> Error ("Undefined variable: " + s)
        | :? System.NotImplementedException -> Error "Not implemented" // FIXME: temporary handled

    member self.Compile (expr: Value): Code =
        Compiler.Compile topLevel expr

    member self.Eval (expr: Value): Value =
        let code = Compiler.Compile topLevel expr
        VM.Execute topLevel code
