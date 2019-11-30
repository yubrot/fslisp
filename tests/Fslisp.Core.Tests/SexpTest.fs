module Fslisp.Core.Tests.Sexp

open Xunit
open Fslisp.Core

[<Fact>]
let sexpTest() =
    Assert.True(Sexp.test (Sexp.Num 123.0))
    Assert.True(Sexp.test Sexp.Nil)
    Assert.True(Sexp.test (Sexp.Bool true))
    Assert.False(Sexp.test (Sexp.Bool false))

[<Fact>]
let sexpPatternConstructionTest() =
    Assert.Equal(
        Sexp.Nil,
        Sexp.List []
    )
    Assert.Equal(
        Sexp.Cons (Sexp.Bool true, Sexp.Nil),
        Sexp.List [Sexp.Bool true]
    )
    Assert.Equal(
        Sexp.Cons (Sexp.Bool true, Sexp.Cons (Sexp.Bool false, Sexp.Nil)),
        Sexp.List [Sexp.Bool true; Sexp.Bool false]
    )
    Assert.Equal(
        Sexp.Cons (Sexp.Sym "quote", Sexp.Cons (Sexp.Bool true, Sexp.Nil)),
        Sexp.Quote (Sexp.Bool true)
    )
    Assert.Equal(
        Sexp.Cons (Sexp.Sym "quasiquote", Sexp.Cons (Sexp.Cons (Sexp.Sym "unquote", Sexp.Cons (Sexp.Sym "foo", Sexp.Nil)), Sexp.Nil)),
        Sexp.Quasiquote (Sexp.Unquote (Sexp.Sym "foo"))
    )
    Assert.Equal(
        Sexp.Cons (Sexp.Sym "unquote-splicing", Sexp.Cons (Sexp.Bool true, Sexp.Nil)),
        Sexp.UnquoteSplicing (Sexp.Bool true)
    )

[<Fact>]
let sexpPatternDeconstructionTest() =
    Assert.Equal(
        Some [],
        match Sexp.List [] with
        | Sexp.List ls -> Some ls
        | _ -> None
    )
    Assert.Equal(
        Some [Sexp.Bool true; Sexp.Bool false],
        match Sexp.List [Sexp.Bool true; Sexp.Bool false] with
        | Sexp.List ls -> Some ls
        | _ -> None
    )
    Assert.Equal(
        Some (Sexp.Sym "foo"),
        match Sexp.Quote (Sexp.Sym "foo") with
        | Sexp.Quote s -> Some s
        | _ -> None
    )
    Assert.Equal(
        Some (Sexp.Sym "foo"),
        match Sexp.Quasiquote (Sexp.Sym "foo") with
        | Sexp.Quasiquote s -> Some s
        | _ -> None
    )
    Assert.Equal(
        Some (Sexp.Sym "foo"),
        match Sexp.Unquote (Sexp.Sym "foo") with
        | Sexp.Unquote s -> Some s
        | _ -> None
    )
    Assert.Equal(
        Some (Sexp.Sym "foo"),
        match Sexp.UnquoteSplicing (Sexp.Sym "foo") with
        | Sexp.UnquoteSplicing s -> Some s
        | _ -> None
    )

[<Fact>]
let sexpPrettyPrint() =
    Assert.Equal("123", Sexp.prettyPrint id (Sexp.Num 123.0))
    Assert.Equal("3.14", Sexp.prettyPrint id (Sexp.Num 3.14))
    Assert.Equal("foo", Sexp.prettyPrint id (Sexp.Sym "foo"))
    Assert.Equal(
        "\"Hello\\nWorld\"",
        Sexp.prettyPrint id (Sexp.Str "Hello\nWorld")
    )
    Assert.Equal(
        "(#t . #f)",
        Sexp.prettyPrint id (Sexp.Cons (Sexp.Bool true, Sexp.Bool false))
    )
    Assert.Equal(
        "(#t #f c)",
        Sexp.prettyPrint id (Sexp.List [Sexp.Bool true; Sexp.Bool false; Sexp.Sym "c"])
    )
    Assert.Equal(
        "()",
        Sexp.prettyPrint id Sexp.Nil
    )
    Assert.Equal(
        "'(#t #f c)",
        Sexp.prettyPrint id (Sexp.Quote (Sexp.List [
            Sexp.Bool true
            Sexp.Bool false
            Sexp.Sym "c"
        ]))
    )
    Assert.Equal(
        "`(#t #f ,c ,@d)",
        Sexp.prettyPrint id (Sexp.Quasiquote (Sexp.List [
            Sexp.Bool true
            Sexp.Bool false
            Sexp.Unquote (Sexp.Sym "c")
            Sexp.UnquoteSplicing (Sexp.Sym "d")
       ]))
    )
