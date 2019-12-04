namespace Fslisp.Core

open System.Collections.Generic

type Compiler(env: Env<Value>) =
    let code = List<Inst<Value>>()

    member _.Complete(): Code<Value> =
        { Instructions = List.ofSeq code }

    member _.Do (inst: Inst<Value>) =
        code.Add inst

    member self.Eval (expr: Value) =
        match expr with
        | Sexp.Sym sym ->
            code.Add (Inst.Ldv sym)
        | Sexp.List (f :: args) ->
            match env.Refer f with
            | Some (Sexp.Pure (Native.Syntax syntax)) ->
                syntax.Compile self args
            | _ ->
                self.Eval f
                args |> List.iter self.Eval
                self.Do (Inst.App (List.length args))
        | Sexp.Cons _ ->
            raise (CompileErrorException ("Improper list: " + expr.ToString()))
        | expr ->
            self.Do (Inst.Ldc expr)

    interface ICompiler with
        member self.Do inst = self.Do inst

        member self.Eval s = self.Eval s

        member self.Block f =
            let compiler = Compiler(env)
            f compiler
            compiler.Complete()

    static member Compile (env: Env<Value>) (s: Value): Code<Value> =
        let compiler = Compiler(env)
        (compiler :> ICompiler).Eval s
        compiler.Complete()
