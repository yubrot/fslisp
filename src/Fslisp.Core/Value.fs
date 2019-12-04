namespace Fslisp.Core

exception SyntaxErrorException of string

exception CompileErrorException of string

[<RequireQualifiedAccess>]
type Native =
    | Syntax of ISyntax
    | Fun of Env<Value> * Pattern * Code<Value>
    | Macro of Env<Value> * Pattern * Code<Value>

    override self.ToString() =
        match self with
        | Syntax _ -> "<syntax>"
        | Fun _ -> "<fun>"
        | Macro _ -> "<macro>"

and Value = Sexp<Native>

and ISyntax =
    abstract MacroExpand : IMacroExpander -> Value list -> Value list
    abstract Compile : ICompiler -> Value list -> unit

and IMacroExpander =
    abstract Expand : recurse:bool -> Value -> Value

and ICompiler =
    abstract Do : Inst<Value> -> unit
    abstract Eval : Value -> unit
    abstract Block : (ICompiler -> unit) -> Code<Value>
