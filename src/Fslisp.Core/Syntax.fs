[<RequireQualifiedAccess>]
module Fslisp.Core.Syntax

type IMacroExpander with
    member self.ExpandArgs bs ss =
        match bs, ss with
        | true :: bs, s :: ss ->
            self.Expand true s :: self.ExpandArgs bs ss
        | false :: bs, s :: ss ->
            s :: self.ExpandArgs bs ss
        | _, rest ->
            rest |> List.map (self.Expand true)

type ICompiler with
    member self.Def sym x =
        self.Eval x
        self.Do (Inst.Def sym)
        self.Do (Inst.Ldc Sexp.Nil)

    member self.Set sym x =
        self.Eval x
        self.Do (Inst.Set sym)
        self.Do (Inst.Ldc Sexp.Nil)

    member self.Begin args =
        match args with
        | [] ->
            self.Do (Inst.Ldc Sexp.Nil)
        | [arg] ->
            self.Eval arg
        | arg :: args ->
            self.Eval arg
            self.Do Inst.Pop
            self.Begin args

    member self.If c t e =
        self.Eval c
        let t = self.Block (fun c -> c.Eval t; c.Do Inst.Leave)
        let e = self.Block (fun c -> c.Eval e; c.Do Inst.Leave)
        self.Do (Inst.Sel (t, e))

    member self.Fun pattern body =
        let body = self.Block (fun c -> c.Begin body; c.Do Inst.Leave)
        self.Do (Inst.Ldf (pattern, body))

    member self.Macro pattern body =
        let body = self.Block (fun c -> c.Begin body)
        self.Do (Inst.Ldm (pattern, body))

    member self.Builtin sym =
        self.Do (Inst.Ldb sym)

    member self.Quote s =
        self.Do (Inst.Ldc s)

type Def() =
    interface ISyntax with
        member __.MacroExpand expander args =
            expander.ExpandArgs [false; true] args
        member __.Compile compiler args =
            match args with
            | [Sexp.Sym sym; x] -> compiler.Def sym x
            | _ -> raise (SyntaxErrorException "expected (def sym x)")

type Set() =
    interface ISyntax with
        member __.MacroExpand expander args =
            expander.ExpandArgs [false; true] args
        member __.Compile compiler args =
            match args with
            | [Sexp.Sym sym; x] -> compiler.Set sym x
            | _ -> raise (SyntaxErrorException "expected (set sym x)")

type Begin() =
    interface ISyntax with
        member __.MacroExpand expander args =
            expander.ExpandArgs [] args
        member __.Compile compiler args =
            compiler.Begin args

type If() =
    interface ISyntax with
        member __.MacroExpand expander args =
            expander.ExpandArgs [true; true; true] args
        member __.Compile compiler args =
            match args with
            | [c; t; e] -> compiler.If c t e
            | _ -> raise (SyntaxErrorException "expected (if cond then else)")

type Fun() =
    interface ISyntax with
        member __.MacroExpand expander args =
            expander.ExpandArgs [false; true] args
        member __.Compile compiler args =
            match args with
            | pattern :: body ->
                match Pattern.build pattern with
                | Ok pattern -> compiler.Fun pattern body
                | Error e -> raise (SyntaxErrorException ("invalid pattern " + e.ToString()))
            | _ -> raise (SyntaxErrorException "expected (fun pattern body...)")

type Macro() =
    interface ISyntax with
        member __.MacroExpand expander args =
            expander.ExpandArgs [false; true] args
        member __.Compile compiler args =
            match args with
            | pattern :: body ->
                match Pattern.build pattern with
                | Ok pattern -> compiler.Macro pattern body
                | Error e -> raise (SyntaxErrorException ("invalid pattern " + e.ToString()))
            | _ -> raise (SyntaxErrorException "expected (macro pattern body...)")

type Builtin() =
    interface ISyntax with
        member __.MacroExpand expander args =
            expander.ExpandArgs [false] args
        member __.Compile compiler args =
            match args with
            | [Sexp.Sym sym] -> compiler.Builtin sym
            | _ -> raise (SyntaxErrorException "expected (builtin sym)")

type Quote() =
    interface ISyntax with
        member __.MacroExpand expander args =
            expander.ExpandArgs [false] args
        member __.Compile compiler args =
            match args with
            | [s] -> compiler.Quote s
            | _ -> raise (SyntaxErrorException "expected (quote expr)")

let install (env: Env<Value>) =
    let bind sym syntax = env.Define sym (Sexp.Pure (Syntax (syntax())))
    bind "def" Def
    bind "set!" Set
    bind "begin" Begin
    bind "if" If
    bind "fun" Fun
    bind "macro" Macro
    bind "builtin" Builtin
    bind "quote" Quote
