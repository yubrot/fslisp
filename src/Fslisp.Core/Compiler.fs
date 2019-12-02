namespace Fslisp.Core

type Compiler(env: Env<Value>) =
    member self.Compile s =
        seq {
            match s with
            | Sexp.Sym sym ->
                yield Inst.Ldv sym
            | Sexp.List (f :: args) ->
                match env.Refer f with
                | Some (Sexp.Pure (Syntax syntax)) ->
                    match syntax.Compile self args with
                    | Ok code -> yield! code
                    | Error e -> raise (EvaluationErrorException ("Syntax error: " + e))
                | _ ->
                    yield! self.Compile f
                    for arg in args do yield! self.Compile arg
                    yield Inst.App (List.length args)
            | Sexp.Cons _ ->
                raise (EvaluationErrorException ("Compile error: improper list: " + s.ToString()))
            | s ->
                yield Inst.Ldc s
        }

    interface ICompiler with
        member self.Compile s = self.Compile s
