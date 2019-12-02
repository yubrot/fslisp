module Fslisp.Core.Syntax

type ICompiler with
    member self.CompileDef sym x =
        seq {
            yield! self.Compile x
            yield Inst.Def sym
            yield Inst.Ldc Sexp.Nil
        }

    member self.CompileSet sym x =
        seq {
            yield! self.Compile x
            yield Inst.Set sym
            yield Inst.Ldc Sexp.Nil
        }

    member self.CompileBegin args =
        match args with
        | [] ->
            seq { Inst.Ldc Sexp.Nil }
        | [arg] ->
            self.Compile arg
        | arg :: args ->
            seq {
                yield! self.Compile arg
                yield Inst.Pop
                yield! self.CompileBegin args
            }

    member self.CompileIf c t e =
        seq {
            yield! self.Compile c
            let t = [ yield! self.Compile t; yield Inst.Leave ]
            let e = [ yield! self.Compile e; yield Inst.Leave ]
            yield Inst.Sel (t, e)
        }

    member self.CompileFun pattern body =
        let body = [ yield! self.CompileBegin body; yield Inst.Leave ]
        seq { Inst.Ldf (pattern, body) }

    member self.CompileMacro pattern body =
        let body = [ yield! self.CompileBegin body ]
        seq { Inst.Ldm (pattern, body) }

    member self.CompileBuiltin sym =
        seq { Inst.Ldb sym }

    member self.CompileQuote s =
        seq { Inst.Ldc s }

type Def() =
    interface ISyntax with
        member __.Compile compiler args =
            match args with
            | [Sexp.Sym sym; x] -> Ok (compiler.CompileDef sym x)
            | _ -> Error "expected (def sym x)"

type Set() =
    interface ISyntax with
        member __.Compile compiler args =
            match args with
            | [Sexp.Sym sym; x] -> Ok (compiler.CompileSet sym x)
            | _ -> Error "expected (set sym x)"

type Begin() =
    interface ISyntax with
        member __.Compile compiler args =
            Ok (compiler.CompileBegin args)

type If() =
    interface ISyntax with
        member __.Compile compiler args =
            match args with
            | [c; t; e] -> Ok (compiler.CompileIf c t e)
            | _ -> Error "expected (if cond then else)"

type Fun() =
    interface ISyntax with
        member __.Compile compiler args =
            match args with
            | pattern :: body ->
                match Pattern.build pattern with
                | Ok pattern -> Ok (compiler.CompileFun pattern body)
                | Error e -> Error ("invalid pattern " + e.ToString())
            | _ -> Error "expected (fun pattern body...)"

type Macro() =
    interface ISyntax with
        member __.Compile compiler args =
            match args with
            | pattern :: body ->
                match Pattern.build pattern with
                | Ok pattern -> Ok (compiler.CompileMacro pattern body)
                | Error e -> Error ("invalid pattern " + e.ToString())
            | _ -> Error "expected (macro pattern body...)"

type Builtin() =
    interface ISyntax with
        member __.Compile compiler args =
            match args with
            | [Sexp.Sym sym] -> Ok (compiler.CompileBuiltin sym)
            | _ -> Error "expected (builtin sym)"

type Quote() =
    interface ISyntax with
        member __.Compile compiler args =
            match args with
            | [s] -> Ok (compiler.CompileQuote s)
            | _ -> Error "expected (quote expr)"

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
