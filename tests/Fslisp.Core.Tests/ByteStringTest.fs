module Fslisp.Core.Tests.ByteString

open System
open Xunit
open Fslisp.Core

[<Fact>]
let equalityTest() =
    Assert.True(
        ByteString.Create([| 1uy; 2uy; 3uy |])
            = ByteString.Create([| 1uy; 2uy; 3uy |])
    )
    Assert.False(
        ByteString.Create([| 1uy; 2uy; 3uy |])
            = ByteString.Create([| 1uy; 2uy |])
    )
    Assert.False(
        ByteString.Create([| 1uy; 4uy; 3uy |])
            = ByteString.Create([| 1uy; 2uy; 3uy |])
    )
    Assert.True(
        ByteString.Create([| 1uy; 4uy; 3uy |])
            = ByteString.Create([| 2uy; 1uy; 4uy; 3uy; 4uy |], 1, 3)
    )

[<Fact>]
let lengthTest() =
    Assert.Equal(
        3,
        ByteString.Create([| 1uy; 2uy; 3uy |]).Length
    )
    Assert.Equal(
        3,
        ByteString.Create([| 2uy; 1uy; 4uy; 3uy; 4uy |], 1, 3).Length
    )

[<Fact>]
let itemTest() =
    Assert.Equal(
        1uy,
        ByteString.Create([| 1uy; 2uy; 3uy |]).[0]
    )
    Assert.Equal(
        2uy,
        ByteString.Create([| 1uy; 2uy; 3uy |]).[1]
    )
    Assert.Equal(
        1uy,
        ByteString.Create([| 2uy; 1uy; 4uy; 3uy; 4uy |], 1, 3).[0]
    )
    Assert.Equal(
        4uy,
        ByteString.Create([| 2uy; 1uy; 4uy; 3uy; 4uy |], 1, 3).[1]
    )
    Assert.Throws<IndexOutOfRangeException>(fun () ->
        ByteString.Create([| 1uy; 2uy; 3uy |], 1, 1).[-1] |> ignore
    ) |> ignore
    Assert.Equal(
        2uy,
        ByteString.Create([| 1uy; 2uy; 3uy |], 1, 1).[0]
    )
    Assert.Throws<IndexOutOfRangeException>(fun () ->
        ByteString.Create([| 1uy; 2uy; 3uy |], 1, 1).[1] |> ignore
    ) |> ignore

[<Fact>]
let sliceTest() =
    Assert.Equal(
        ByteString.Create([| 1uy; 2uy; 3uy |]),
        ByteString.Create([| 1uy; 2uy; 3uy |]).[0..2]
    )
    Assert.Equal(
        ByteString.Create([| 1uy; 2uy |]),
        ByteString.Create([| 1uy; 2uy; 3uy |]).[0..1]
    )
    Assert.Equal(
        ByteString.Create([| 2uy; 3uy |]),
        ByteString.Create([| 1uy; 2uy; 3uy |]).[1..2]
    )
    Assert.Equal(
        ByteString.Create([| 2uy; 3uy |]),
        ByteString.Create([| 1uy; 2uy; 3uy; 4uy |]).[0..2].[1..2]
    )
    Assert.Equal(
        ByteString.Create([| 2uy; 3uy |]),
        ByteString.Create([| 1uy; 2uy; 3uy; 4uy |]).[1..3].[0..1]
    )
    Assert.Equal(
        ByteString.Create([| 3uy; 4uy |]),
        ByteString.Create([| 1uy; 2uy; 3uy; 4uy |]).[1..3].[1..2]
    )
    Assert.Throws<IndexOutOfRangeException>(fun () ->
        ByteString.Create([| 1uy; 2uy; 3uy |]).[1..3] |> ignore
    ) |> ignore
    Assert.Throws<IndexOutOfRangeException>(fun () ->
        ByteString.Create([| 1uy; 2uy; 3uy |]).[1..2].[-1..1] |> ignore
    ) |> ignore
    Assert.Throws<IndexOutOfRangeException>(fun () ->
        ByteString.Create([| 1uy; 2uy; 3uy |]).[1..2].[1..2] |> ignore
    ) |> ignore

[<Fact>]
let comparisonTest() =
    Assert.True(
        ByteString.Create([| 1uy; 2uy; 3uy |])
            < ByteString.Create([| 1uy; 2uy; 4uy |])
    )
    Assert.True(
        ByteString.Create([| 1uy; 3uy; 3uy |])
            > ByteString.Create([| 1uy; 2uy; 4uy |])
    )
    Assert.True(
        ByteString.Create([| 1uy; 3uy |])
            < ByteString.Create([| 1uy; 3uy; 0uy |])
    )
    Assert.True(
        ByteString.Create([| 5uy; 3uy |])
            > ByteString.Create([| 1uy; 3uy; 0uy |])
    )
    Assert.True(
        ByteString.Create([| 5uy; 3uy |])
            <= ByteString.Create([| 5uy; 3uy; 0uy |])
    )
    Assert.True(
        ByteString.Create([| 5uy; 3uy |])
            >= ByteString.Create([| 5uy; 3uy |])
    )

[<Fact>]
let encodeDecodeTest() =
    Assert.Equal(
        "Hello, World!\n",
        "Hello, World!\n" |> ByteString.encode |> ByteString.decode
    )
