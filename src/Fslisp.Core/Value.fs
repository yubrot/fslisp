namespace Fslisp.Core

exception InternalErrorException of string
exception EvaluationErrorException of string

type Value = Sexp<Native>

and Code = Code<Native>

and Native =
    | Syntax of ISyntax

    override self.ToString() =
        match self with
        | Syntax _ -> "<syntax>"

and ISyntax =
    abstract Compile : ICompiler -> Value list -> Result<Code, string>

and ICompiler =
    abstract Compile : Value -> Code
