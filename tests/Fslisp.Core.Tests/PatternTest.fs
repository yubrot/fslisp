module Fslisp.Core.Tests.Pattern

open Xunit
open Fslisp.Core

[<Fact>]
let patternToStringTest() =
    Assert.Equal("()", { Fixed = []; Rest = None }.ToString())
    Assert.Equal("(a)", { Fixed = ["a"]; Rest = None }.ToString())
    Assert.Equal("(a b)", { Fixed = ["a"; "b"]; Rest = None }.ToString())
    Assert.Equal("(a b)", { Fixed = ["a"; "b"]; Rest = None }.ToString())
    Assert.Equal("(a b . c)", { Fixed = ["a"; "b"]; Rest = Some "c" }.ToString())
    Assert.Equal("(a . c)", { Fixed = ["a"]; Rest = Some "c" }.ToString())
    Assert.Equal("c", { Fixed = []; Rest = Some "c" }.ToString())

[<Fact>]
let patternBuildTest() =
    Assert.Equal(
        Ok { Fixed = []; Rest = None },
        Pattern.build Sexp.Nil
    )
    Assert.Equal(
        Error (Sexp.Num 0.0),
        Pattern.build (Sexp.Num 0.0)
    )
    Assert.Equal(
        Ok { Fixed = ["a"]; Rest = None },
        Pattern.build (Sexp.List [Sexp.Sym "a"])
    )
    Assert.Equal(
        Ok { Fixed = ["a"; "b"]; Rest = None },
        Pattern.build (Sexp.List [Sexp.Sym "a"; Sexp.Sym "b"])
    )
    Assert.Equal(
        Error (Sexp.Num 0.0),
        Pattern.build (Sexp.List [Sexp.Sym "a"; Sexp.Num 0.0])
    )
    Assert.Equal(
        Ok { Fixed = ["a"; "b"]; Rest = Some "c" },
        Pattern.build (Sexp.ListLike [Sexp.Sym "a"; Sexp.Sym "b"] (Sexp.Sym "c"))
    )
    Assert.Equal(
        Ok { Fixed = []; Rest = Some "c" },
        Pattern.build (Sexp.Sym "c")
    )

[<Fact>]
let patternBindTest() =
    let exact2 = { Fixed = ["a"; "b"]; Rest = None }
    let atLeast1 = { Fixed = ["a"]; Rest = Some "b" }
    let testcases = [
        [], [
            exact2, None
            atLeast1, None
        ]
        [Sexp.Num 0.0], [
            exact2, None
            atLeast1, Some ["a", Sexp.Num 0.0; "b", Sexp.Nil]
        ]
        [Sexp.Num 0.0; Sexp.Num 1.0], [
            exact2, Some ["a", Sexp.Num 0.0; "b", Sexp.Num 1.0]
            atLeast1, Some ["a", Sexp.Num 0.0; "b", Sexp.List [Sexp.Num 1.0]]
        ]
        [Sexp.Num 0.0; Sexp.Num 1.0; Sexp.Num 2.0], [
            exact2, None
            atLeast1, Some ["a", Sexp.Num 0.0; "b", Sexp.List [Sexp.Num 1.0; Sexp.Num 2.0]]
        ]
    ]
    for input, patternAndResults in testcases do
        for pattern, result in patternAndResults do
            match result, Pattern.bind pattern input with
            | None, Error _ ->
                ()
            | Some r, Ok map ->
                Assert.Equal<Map<string,Sexp<unit>>>(Map.ofList r, map)
            | None, Ok _ ->
                Assert.True(false, sprintf "%A <- %A: Success" pattern input)
            | Some _, Error e ->
                Assert.True(false, sprintf "%A <- %A: %s" pattern input e)
