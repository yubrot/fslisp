module Fslisp.Core.Tests.Signature

open Xunit
open System.Text
open Fslisp.Core
open Signature

[<Fact>]
let numSignatureTest() =
    let p = Num "count"
    Assert.Equal(Ok 123.0, tryMatch p (Sexp.Num 123.0))
    Assert.Equal(Error (Sexp.Sym "count", TypeMismatch), tryMatch p Sexp.Nil)

[<Fact>]
let symSignatureTest() =
    let p = Sym "var"
    Assert.Equal(Ok "foo", tryMatch p (Sexp.Sym "foo"))
    Assert.Equal(Error (Sexp.Sym "var", TypeMismatch), tryMatch p (Sexp.Num 123.0))

[<Fact>]
let strSignatureTest() =
    let p = Str "body"
    let str = Encoding.UTF8.GetBytes "foo"
    Assert.Equal(Ok str, tryMatch p (Sexp.Str str))
    Assert.Equal(Error (Sexp.Sym "body", TypeMismatch), tryMatch p (Sexp.Num 123.0))

[<Fact>]
let consSignatureTest() =
    let p = Cons (Sym "car", Sym "cdr")
    Assert.Equal(Ok ("a", "b"), tryMatch p (Sexp.Cons (Sexp.Sym "a", Sexp.Sym "b")))
    Assert.Equal(Error (Sexp.Cons (Sexp.Sym "car", Sexp.Sym "cdr"), TypeMismatch), tryMatch p (Sexp.Num 123.0))

[<Fact>]
let nilSignatureTest() =
    let p = Nil
    Assert.Equal(Ok (), tryMatch p Sexp.Nil)
    Assert.Equal(Error (Sexp.Nil, TypeMismatch), tryMatch p (Sexp.Num 123.0))

[<Fact>]
let boolSignatureTest() =
    let p = Bool "on"
    Assert.Equal(Ok true, tryMatch p (Sexp.Bool true))
    Assert.Equal(Error (Sexp.Sym "on", TypeMismatch), tryMatch p (Sexp.Num 123.0))

[<Fact>]
let listSignatureTest() =
    let p = List (Sym "foo")
    Assert.Equal(
        Ok ["a"; "b"],
        tryMatch p (Sexp.List [Sexp.Sym "a"; Sexp.Sym "b"])
    )
    Assert.Equal(
        Error (Sexp.List [Sexp.Sym "foo"; Sexp.Sym "..."], TypeMismatch),
        tryMatch p (Sexp.ListLike [Sexp.Sym "a"] (Sexp.Sym "b"))
    )

[<Fact>]
let patSignatureTest() =
    let p = Pat "pattern"
    Assert.Equal(
        Ok { Fixed = ["a"; "b"]; Rest = None },
        tryMatch p (Sexp.List [Sexp.Sym "a"; Sexp.Sym "b"])
    )
    Assert.Equal(
        Error (Sexp.Sym "pattern", InvalidPattern "123"),
        tryMatch p (Sexp.List [Sexp.Num 123.0])
    )

[<Fact>]
let tupleSignatureTest() =
    Assert.Equal(
        Ok (),
        tryMatch () []
    )
    Assert.Equal(
        Error ([], ArityMismatch),
        tryMatch () [Sexp.Sym "a"]
    )
    Assert.Equal(
        Ok ("test", ()),
        tryMatch (Sym "a", ()) [Sexp.Sym "test"]
    )
    Assert.Equal(
        Error ([Sexp.Sym "a"], ArityMismatch),
        tryMatch (Sym "a", ()) [Sexp.Sym "test"; Sexp.Sym "over"]
    )
    Assert.Equal(
        Ok ("test", "test2", ()),
        tryMatch (Sym "a", Sym "b", ()) [Sexp.Sym "test"; Sexp.Sym "test2"]
    )
    Assert.Equal(
        Error ([Sexp.Sym "a"; Sexp.Sym "b"], ArityMismatch),
        tryMatch (Sym "a", Sym "b", ()) [Sexp.Sym "test"]
    )
    let text = Encoding.UTF8.GetBytes "test"
    Assert.Equal(
        Ok (1.0, "foo", text, true, ()),
        tryMatch (Num "a", Sym "b", Str "c", Bool "d", ()) [Sexp.Num 1.0; Sexp.Sym "foo"; Sexp.Str text; Sexp.Bool true]
    )

[<Fact>]
let placeholderSignatureTest() =
    Assert.Equal(
        Ok (Sexp.Sym "f", Sexp.Bool true, ()),
        tryMatch ("one", "two", ()) [Sexp.Sym "f"; Sexp.Bool true]
    )

[<Fact>]
let restSignatureTest() =
    Assert.Equal(
        Ok (Sexp.Num 1.0, []),
        tryMatch ("head", Rest "tail") [Sexp.Num 1.0]
    )
    Assert.Equal(
        Ok (Sexp.Num 1.0, [Sexp.Num 2.0; Sexp.Num 3.0]),
        tryMatch ("head", Rest "tail") [Sexp.Num 1.0; Sexp.Num 2.0; Sexp.Num 3.0]
    )
