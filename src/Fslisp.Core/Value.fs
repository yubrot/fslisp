namespace Fslisp.Core

exception EvaluationErrorException of string

exception InternalErrorException of string

exception SyntaxErrorException of string

exception CompileErrorException of string

[<RequireQualifiedAccess>]
type Native =
    | Builtin of IBuiltin
    | Syntax of ISyntax
    | Fun of Env<Value> * Pattern * Code<Value>
    | Macro of Env<Value> * Pattern * Code<Value>
    | Port of System.IO.Stream
    | Vec of Value[]

    override self.ToString() =
        match self with
        | Builtin _ -> "<builtin>"
        | Syntax _ -> "<syntax>"
        | Fun _ -> "<fun>"
        | Macro _ -> "<macro>"
        | Port _ -> "<port>"
        | Vec a -> (Sexp.List (Sexp.Sym "vec" :: List.ofArray a)).ToString()

and Value = Sexp<Native>

and IBuiltin =
    abstract Run : IVM -> Value list -> unit

and ISyntax =
    abstract MacroExpand : IMacroExpander -> Value list -> Value list
    abstract Compile : ICompiler -> Value list -> unit

and IVM =
    abstract Push : Value -> unit
    abstract Apply : Value -> Value list -> unit
    abstract ApplyNever : Value  -> Value list -> unit
    abstract ApplyCont : Cont -> unit
    abstract CaptureCont : unit -> Cont
    abstract Context : IContext

and Cont =
    { Stack: Value list
      Env: Env<Value>
      Code: Code<Value>
      Dump: (Env<Value> * Code<Value>) list }

and IMacroExpander =
    abstract Expand : recurse:bool -> Value -> Value

and ICompiler =
    abstract Do : Inst<Value> -> unit
    abstract Eval : Value -> unit
    abstract Block : (ICompiler -> unit) -> Code<Value>

and IContext =
    abstract TopLevel : Env<Value>
    abstract Builtins : IBuiltinRegistry

and IBuiltinRegistry =
    abstract Get : string -> IBuiltin option

[<RequireQualifiedAccess>]
module Native =
    let isProc n =
        match n with
        | Native.Builtin _
        | Native.Fun _ -> true
        | _ -> false

    let isMeta n =
        match n with
        | Native.Syntax _
        | Native.Macro _ -> true
        | _ -> false

    let isPort n =
        match n with
        | Native.Port _ -> true
        | _ -> false

    let isVec n =
        match n with
        | Native.Vec _ -> true
        | _ -> false
