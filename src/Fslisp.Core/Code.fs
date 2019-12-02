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

[<RequireQualifiedAccess>]
module CodePrinter =
    let printToString code =
        let mutable blockId = 0
        let blocks = new List<StringBuilder>()

        let (<+) (buf: StringBuilder) (a: obj) = buf.Append(a)

        let rec printInst (buf: StringBuilder) inst =
            match inst with
            | Inst.Ldc c ->
                buf <+ "ldc " <+ c
            | Inst.Ldv v ->
                buf <+ "ldv " <+ v
            | Inst.Ldf (pattern, code) ->
                buf <+ "ldf " <+ printCode ("fun " + pattern.ToString()) code
            | Inst.Ldm (pattern, code) ->
                buf <+ "ldm " <+ printCode ("macro " + pattern.ToString()) code
            | Inst.Ldb s ->
                buf <+ "ldb " <+ s
            | Inst.Sel (a, b) ->
                let a = printCode "then" a
                let b = printCode "else" b
                buf <+ "sel " <+ a <+ " " <+ b
            | Inst.App n ->
                buf <+ "app " <+ n
            | Inst.Leave ->
                buf <+ "leave"
            | Inst.Pop ->
                buf <+ "pop"
            | Inst.Def s ->
                buf <+ "def " <+ s
            | Inst.Set s ->
                buf <+ "set " <+ s

        and printCode header code =
            let id = sprintf "[%d %s]" blockId header
            let buf = StringBuilder()
            blockId <- blockId + 1
            blocks.Add(buf)
            buf <+ id <+ "\n" |> ignore
            for inst in code do
                buf <+ "  " |> ignore
                printInst buf inst |> ignore
                buf <+ "\n" |> ignore
            id

        printCode "entry" code |> ignore

        let buf = Seq.fold (<+) (StringBuilder()) blocks
        buf.ToString()
