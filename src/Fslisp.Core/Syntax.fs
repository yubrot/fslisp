[<RequireQualifiedAccess>]
module Fslisp.Core.Syntax

open Signature

type ICompiler with
    member self.Def (sym: string) (x: Value) =
        self.Eval x
        self.Do(Inst.Def sym)
        self.Do(Inst.Ldc Sexp.Nil)

    member self.Set (sym: string) (x: Value) =
        self.Eval x
        self.Do(Inst.Set sym)
        self.Do(Inst.Ldc Sexp.Nil)

    member self.Begin(args: Value list) =
        match args with
        | [] -> self.Do(Inst.Ldc Sexp.Nil)
        | [ arg ] -> self.Eval arg
        | arg :: args ->
            self.Eval arg
            self.Do Inst.Pop
            self.Begin args

    member self.If (c: Value) (t: Value) (e: Value) =
        self.Eval c

        let t =
            self.Block(fun c ->
                c.Eval t
                c.Do Inst.Leave)

        let e =
            self.Block(fun c ->
                c.Eval e
                c.Do Inst.Leave)

        self.Do(Inst.Sel(t, e))

    member self.Fun (pattern: Pattern) (body: Value list) =
        let body =
            self.Block(fun c ->
                c.Begin body
                c.Do Inst.Leave)

        self.Do(Inst.Ldf(pattern, body))

    member self.Macro (pattern: Pattern) (body: Value list) =
        let body = self.Block(fun c -> c.Begin body)
        self.Do(Inst.Ldm(pattern, body))

    member self.Builtin(sym: string) = self.Do(Inst.Ldb sym)

    member self.Quote(expr: Value) = self.Do(Inst.Ldc expr)

let inline syntax signature arm =
    { new ISyntax with
        member _.MacroExpand expander expr =
            let rec go bs ss =
                match bs, ss with
                | true :: bs, s :: ss -> expander.Expand true s :: go bs ss
                | false :: bs, s :: ss -> s :: go bs ss
                | _, rest -> rest |> List.map (expander.Expand true)

            go signature expr

        member _.Compile compiler expr =
            match handle [ arm ] compiler expr with
            | Ok() -> ()
            | Error e -> raise (SyntaxErrorException e) }

let install (env: Env<Value>) =
    let inline define sym signature arm =
        env.Define sym (Sexp.Pure(Native.Syntax(syntax signature arm)))

    define "def" [ false; false; true ]
    <| ("def", Sym "sym", "x", ())
       ->> fun compiler (_, sym, x, ()) -> compiler.Def sym x

    define "set!" [ false; false; true ]
    <| ("set!", Sym "sym", "x", ())
       ->> fun compiler (_, sym, x, ()) -> compiler.Set sym x

    define "begin" [ false ]
    <| ("begin", Rest "body") ->> fun compiler (_, body) -> compiler.Begin body

    define "if" [ false; true; true; true ]
    <| ("if", "cond", "then", "else", ())
       ->> fun compiler (_, c, t, e, ()) -> compiler.If c t e

    define "fun" [ false; false ]
    <| ("fun", Pat "pattern", Rest "body")
       ->> fun compiler (_, pattern, body) -> compiler.Fun pattern body

    define "macro" [ false; false ]
    <| ("macro", Pat "pattern", Rest "body")
       ->> fun compiler (_, pattern, body) -> compiler.Macro pattern body

    define "builtin" [ false; false ]
    <| ("builtin", Sym "sym", ()) ->> fun compiler (_, sym, ()) -> compiler.Builtin sym

    define "quote" [ false; false ]
    <| ("quote", "expr", ()) ->> fun compiler (_, expr, ()) -> compiler.Quote expr
