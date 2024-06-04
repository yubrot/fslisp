namespace Fslisp.Core

type Context(builtinRegistry: IBuiltinRegistry) =
    let topLevel = Env(None)
    do Syntax.install topLevel

    interface IContext with
        member _.TopLevel = topLevel
        member _.Builtins = builtinRegistry

[<AutoOpen>]
module Context =
    let private wrap f =
        try
            Ok(f ())
        with
        | SyntaxErrorException e -> Error("Syntax error: " + e)
        | CompileErrorException e -> Error("Compile error: " + e)
        | EvaluationErrorException e -> Error("Evaluation error: " + e)
        | InternalErrorException e -> Error("Internal error: " + e)
        | UndefinedVariableException s -> Error("Undefined variable: " + s)

    type IContext with
        member self.Compile(expr: Value) : Result<Code<Value>, string> =
            wrap (fun () -> Compiler.Compile self.TopLevel expr)

        member self.MacroExpand (recurse: bool) (expr: Value) : Result<Value, string> =
            wrap (fun () -> MacroExpander.MacroExpand self recurse expr)

        member self.Eval(expr: Value) : Result<Value, string> =
            wrap (fun () ->
                let expr = MacroExpander.MacroExpand self true expr
                let code = Compiler.Compile self.TopLevel expr
                VM.Execute self self.TopLevel code)
