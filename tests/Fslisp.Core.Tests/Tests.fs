module Tests

open Xunit
open Fslisp.Core

[<Fact>]
let ``My test``() = Assert.Equal("Hello test", Say.hello "test")
