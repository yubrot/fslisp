module Fslisp.Core.Tests.Parser

open Xunit
open Fslisp.Core
open Parser

[<Fact>]
let parseAmbientTest() =
    Assert.Equal(
        Ok Sexp.Nil,
        parseToEnd sexp "test" "()"
    )
    Assert.Equal(
        Ok Sexp.Nil,
        parseToEnd sexp "test" "   \t  \n  ()\n"
    )
    Assert.Equal(
        Ok Sexp.Nil,
        parseToEnd sexp "test" "; comment\n(); comment"
    )

[<Fact>]
let parseAtomTest() =
    Assert.Equal(
        Ok Sexp.Nil,
        parseToEnd sexp "test" "()"
    )
    Assert.Equal(
        Ok (Sexp.Num 123.0),
        parseToEnd sexp "test" "123"
    )
    Assert.Equal(
        Ok (Sexp.Num 3.14),
        parseToEnd sexp "test" "3.14"
    )
    Assert.Equal(
        Ok (Sexp.Sym "foo"),
        parseToEnd sexp "test" "foo"
    )
    Assert.Equal(
        Ok (Sexp.Sym "*foo-bar+baz"),
        parseToEnd sexp "test" "*foo-bar+baz"
    )
    Assert.Equal(
        Ok (Sexp.Str "Hello, World!\n"),
        parseToEnd sexp "test" "\"Hello, World!\\n\""
    )
    Assert.Equal(
        Ok (Sexp.Str " \" \\ \r \t "),
        parseToEnd sexp "test" "\" \\\" \\\\ \\r \\t \""
    )
    Assert.Equal(
        Ok (Sexp.Bool true),
        parseToEnd sexp "test" "#t"
    )
    Assert.Equal(
        Ok (Sexp.Bool false),
        parseToEnd sexp "test" "#f"
    )

[<Fact>]
let parseListTest() =
    Assert.Equal(
        Ok (Sexp.List [Sexp.Bool true]),
        parseToEnd sexp "test" "(#t)"
    )
    Assert.Equal(
        Ok (Sexp.List [Sexp.Bool true; Sexp.Bool false; Sexp.Bool true]),
        parseToEnd sexp "test" "(#t #f #t)"
    )
    Assert.Equal(
        Ok (Sexp.ListLike [Sexp.Bool true; Sexp.Bool false] (Sexp.Bool true)),
        parseToEnd sexp "test" "(#t #f . #t)"
    )
    Assert.Equal(
        Ok (Sexp.List [Sexp.Num 1.0; Sexp.List [Sexp.Sym "b"; Sexp.List [Sexp.Str "c"]]]),
        parseToEnd sexp "test" "[1 (b [\"c\"])]"
    )
    Assert.Equal(
        Ok (Sexp.Quasiquote (Sexp.List [
            Sexp.Quote (Sexp.Sym "foo")
            Sexp.Unquote (Sexp.Sym "bar")
            Sexp.UnquoteSplicing (Sexp.List [Sexp.Sym "hoge"; Sexp.Sym "fuga"])
        ])),
        parseToEnd sexp "test" "`('foo ,bar ,@(hoge fuga))"
    )

[<Fact>]
let parseProgramTest() =
    Assert.Equal(
        Ok ([
            Sexp.Num 123.0
            Sexp.Nil
            Sexp.Sym "foo"
            Sexp.Str "bar"
        ]),
        parseToEnd program "test" "123 () foo ; test \n \"bar\""
    )

[<Fact>]
let contiguousParseTest() =
    let parse = parse sexp "test" "   123   456   789  "
    Assert.Equal(Ok (Sexp.Num 123.0, 9), parse None)
    Assert.Equal(Ok (Sexp.Num 456.0, 15), parse (Some 9))
    Assert.Equal(Ok (Sexp.Num 789.0, 20), parse (Some 15))
