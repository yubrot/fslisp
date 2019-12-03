namespace Fslisp.Core

exception SyntaxErrorException of string

exception CompileErrorException of string

type Value = Sexp<Native>

and Inst = Inst<Native>

and Code = Code<Native>

and Native =
    | Syntax of ISyntax
    | Fun of Env<Value> * Pattern * Code
    | Macro of Env<Value> * Pattern * Code

    override self.ToString() =
        match self with
        | Syntax _ -> "<syntax>"
        | Fun _ -> "<fun>"
        | Macro _ -> "<macro>"

and ISyntax =
    abstract MacroExpand : IMacroExpander -> Value list -> Value list
    abstract Compile : ICompiler -> Value list -> unit

and IMacroExpander =
    abstract Expand : recurse:bool -> Value -> Value

and ICompiler =
    abstract Do : Inst -> unit
    abstract Eval : Value -> unit
    abstract Block : (ICompiler -> unit) -> Code
