module Fslisp.Core.Tests.Env

open Xunit
open Fslisp.Core

[<Fact>]
let singleEnvTest() =
    let env = Env(None)

    Assert.Equal(None, env.Find "foo")
    Assert.Throws<UndefinedVariableException>(fun () ->
        env.Get "foo" |> ignore
    ) |> ignore
    Assert.Throws<UndefinedVariableException>(fun () ->
        env.Set "foo" "value"
    ) |> ignore

    env.Define "foo" "value"

    Assert.Equal(Some "value", env.Find "foo")
    Assert.Equal("value", env.Get "foo")

    env.Set "foo" "new-value"

    Assert.Equal(Some "new-value", env.Find "foo")
    Assert.Equal("new-value", env.Get "foo")

[<Fact>]
let chainedEnvTest() =
    let parent = Env(None)
    let child = Env(Some parent)
    parent.Define "foo" "value"

    Assert.Equal(Some "value", child.Find "foo")
    Assert.Equal("value", child.Get "foo")

    child.Set "foo" "new-value"

    Assert.Equal(Some "new-value", child.Find "foo")
    Assert.Equal(Some "new-value", parent.Find "foo")
    Assert.Throws<UndefinedVariableException>(fun () ->
        child.Set "bar" "value"
    ) |> ignore

    child.Define "bar" "value"

    Assert.Equal(Some "value", child.Find "bar")
    Assert.Equal(None, parent.Find "bar")

    child.Set "bar" "new-value"

    Assert.Equal(Some "new-value", child.Find "bar")
    Assert.Equal(None, parent.Find "bar")

    child.Define "foo" "shadowing"

    Assert.Equal(Some "shadowing", child.Find "foo")
    Assert.Equal(Some "new-value", parent.Find "foo")

    child.Set "foo" "new-shadowing"

    Assert.Equal(Some "new-shadowing", child.Find "foo")
    Assert.Equal(Some "new-value", parent.Find "foo")
