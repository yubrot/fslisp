namespace Fslisp.Core

open System.Collections.Generic
open System.Text

[<RequireQualifiedAccess>]
type Inst<'T> =
    | Ldc of Sexp<'T>
    | Ldv of string
    | Ldf of Pattern * Code<'T>
    | Ldm of Pattern * Code<'T>
    | Ldb of string
    | Sel of Code<'T> * Code<'T>
    | App of int
    | Leave
    | Pop
    | Def of string
    | Set of string

and Code<'T> = Inst<'T> seq

module CodePrinter =
    let printToString code =
        let mutable blockId = 0
        let blocks = new List<StringBuilder>()

        let rec instToString inst =
            match inst with
            | Inst.Ldc c ->
                "ldc " + c.ToString()
            | Inst.Ldv v ->
                "ldv " + v
            | Inst.Ldf (pattern, code) ->
                "ldf " + addCode ("fun " + pattern.ToString()) code
            | Inst.Ldm (pattern, code) ->
                "ldm " + addCode ("macro " + pattern.ToString()) code
            | Inst.Ldb s ->
                "ldb " + s
            | Inst.Sel (a, b) ->
                let a = addCode "then" a
                let b = addCode "else" b
                sprintf "sel %s %s" a b
            | Inst.App n ->
                sprintf "app %d" n
            | Inst.Leave ->
                "leave"
            | Inst.Pop ->
                "pop"
            | Inst.Def s ->
                "def " + s
            | Inst.Set s ->
                "set " + s

        and addCode header code =
            let id = sprintf "[%d %s]" blockId header
            let buf = StringBuilder()
            blockId <- blockId + 1
            blocks.Add(buf)
            buf.Append(id + "\n") |> ignore
            for inst in code do
                inst |> instToString |> sprintf "  %s\n" |> buf.Append |> ignore
            id

        addCode "entry" code |> ignore
        blocks |> Seq.map (fun block -> block.ToString()) |> String.concat ""
