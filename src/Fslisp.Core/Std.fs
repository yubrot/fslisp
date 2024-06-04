module Fslisp.Core.Std

open System
open System.IO
open Signature

let builtin arms =
    { new IBuiltin with
        member _.Run vm expr =
            match handle arms vm expr with
            | Ok() -> ()
            | Error e -> raise (EvaluationErrorException e) }

let builtinGensym (name: string) =
    let mutable id = 0

    builtin
        [ (name, ())
          ->> fun vm (_, ()) ->
              id <- id + 1
              vm.Push(Sexp.Sym(sprintf "#sym.%d" id)) ]

let builtinTest (name: string, f) =
    builtin [ (name, "expr", ()) ->> fun vm (_, expr, ()) -> vm.Push(Sexp.Bool(f expr)) ]

let builtinCompare (name: string, numOp, strOp) =
    let rec go compare x xs =
        match xs with
        | [] -> true
        | y :: xs -> compare x y && go compare y xs

    builtin
        [ (name, ()) ->> fun vm (_, ()) -> vm.Push(Sexp.Bool true)
          (name, Num "num", Rest(Num "nums"))
          ->> fun vm (_, a, bs) -> vm.Push(Sexp.Bool(go numOp a bs))
          (name, Str "str", Rest(Str "strs"))
          ->> fun vm (_, a, bs) -> vm.Push(Sexp.Bool(go strOp a bs)) ]

let private tryIO f =
    try
        Sexp.Cons(Sexp.Bool true, f ())
    with :? SystemException as e ->
        Sexp.Cons(Sexp.Bool false, Sexp.Str(ByteString.encode e.Message))

let builtinCont (cont: Cont) =
    builtin
        [ ("continuation", ())
          ->> fun vm (_, ()) ->
              vm.ApplyCont cont
              vm.Push Sexp.Nil
          ("continuation", "x", ())
          ->> fun vm (_, x, ()) ->
              vm.ApplyCont cont
              vm.Push x
          ("continuation", Rest "xs")
          ->> fun vm (_, _) -> raise (EvaluationErrorException "Multiple values are not implemented") ]

let builtinArgs (name: string, args: string list) =
    let args = args |> List.map (ByteString.encode >> Sexp.Str) |> Sexp.List
    builtin [ ("args", ()) ->> fun vm (_, ()) -> vm.Push args ]

let install (args: string list) (registry: BuiltinRegistry) =
    let register f name arms = registry.Register name (f arms)

    register builtin "cons" [ ("cons", "car", "cdr", ()) ->> fun vm (_, a, b, ()) -> vm.Push(Sexp.Cons(a, b)) ]

    register builtin "exit"
    <| [ ("exit", ()) ->> fun vm (_, ()) -> exit 0
         ("exit", Num "exitcode", ()) ->> fun vm (_, c, ()) -> exit (int c) ]

    register builtin "error"
    <| [ ("error", ())
         ->> fun vm (_, ()) -> raise (EvaluationErrorException "error called")
         ("error", Str "msg", ())
         ->> fun vm (_, msg, ()) -> raise (EvaluationErrorException(ByteString.decode msg)) ]

    register builtinGensym "gensym" "gensym"

    register builtin "car"
    <| [ ("car", Cons("a", "b"), ()) ->> fun vm (_, (a, _), ()) -> vm.Push a ]

    register builtin "cdr"
    <| [ ("cdr", Cons("a", "b"), ()) ->> fun vm (_, (_, b), ()) -> vm.Push b ]

    register builtin "apply"
    <| [ ("apply", "f", List "argument-list", ())
         ->> fun vm (_, f, args, ()) -> vm.Apply(f, args) ]

    register builtinTest "num?" ("num?", Sexp.isNum)
    register builtinTest "sym?" ("sym?", Sexp.isSym)
    register builtinTest "str?" ("str?", Sexp.isStr)
    register builtinTest "cons?" ("cons?", Sexp.isCons)
    register builtinTest "nil?" ("nil?", Sexp.isNil)
    register builtinTest "bool?" ("bool?", Sexp.isBool)
    register builtinTest "proc?" ("proc?", Sexp.isPure Native.isProc)
    register builtinTest "meta?" ("meta?", Sexp.isPure Native.isMeta)
    register builtinTest "port?" ("port?", Sexp.isPure Native.isPort)
    register builtinTest "vec?" ("vec?", Sexp.isPure Native.isVec)

    register builtin "+"
    <| [ ("+", Rest(Num "nums"))
         ->> fun vm (_, nums) -> vm.Push(Sexp.Num(Seq.fold (+) 0.0 nums)) ]

    register builtin "-"
    <| [ ("-", Num "num", ()) ->> fun vm (_, num, ()) -> vm.Push(Sexp.Num(-num))
         ("-", Num "num", Rest(Num "nums"))
         ->> fun vm (_, a, bs) -> vm.Push(Sexp.Num(Seq.fold (-) a bs)) ]

    register builtin "*"
    <| [ ("*", Rest(Num "nums"))
         ->> fun vm (_, nums) -> vm.Push(Sexp.Num(Seq.fold (*) 1.0 nums)) ]

    register builtin "/"
    <| [ ("/", Num "num", ()) ->> fun vm (_, num, ()) -> vm.Push(Sexp.Num(1.0 / num))
         ("/", Num "num", Rest(Num "nums"))
         ->> fun vm (_, a, bs) -> vm.Push(Sexp.Num(Seq.fold (/) a bs)) ]

    register builtin "%"
    <| [ ("%", Num "num", Rest(Num "nums"))
         ->> fun vm (_, a, bs) -> vm.Push(Sexp.Num(Seq.fold (%) a bs)) ]

    register builtin "="
    <| [ ("=", ()) ->> fun vm (_, ()) -> vm.Push(Sexp.Bool true)
         ("=", "x", Rest "xs")
         ->> fun vm (_, x, xs) -> vm.Push(Sexp.Bool(Seq.forall (Sexp.equal x) xs)) ]

    register builtinCompare "<" ("<", (<), (<))
    register builtinCompare ">" (">", (>), (>))
    register builtinCompare "<=" ("<=", (<=), (<=))
    register builtinCompare ">=" (">=", (>=), (>=))

    register builtin "call/cc"
    <| [ ("call/cc", "f", ())
         ->> fun vm (_, f, ()) ->
             let cont = vm.CaptureCont()
             vm.Apply(f, [ Sexp.Pure(Native.Builtin(builtinCont cont)) ]) ]

    register builtin "never"
    <| [ ("never", "f", Rest "args") ->> fun vm (_, f, args) -> vm.ApplyNever(f, args) ]

    register builtin "str"
    <| [ ("str", Rest(Num "bytes"))
         ->> fun vm (_, bytes) ->
             let str =
                 bytes
                 |> Seq.map (fun i ->
                     if i < 0.0 || 256.0 <= i then
                         raise (EvaluationErrorException "Each byte of string must be inside the range 0-255")
                     else
                         byte i)
                 |> Array.ofSeq
                 |> ByteString.Create

             vm.Push(Sexp.Str str) ]

    register builtin "str-ref"
    <| [ ("str-ref", Str "string", Num "index", ())
         ->> fun vm (_, str, index, ()) ->
             let s =
                 try
                     Sexp.Num(float str.[int index])
                 with :? IndexOutOfRangeException ->
                     Sexp.Nil

             vm.Push s ]

    register builtin "str-bytesize"
    <| [ ("str-bytesize", Str "string", ())
         ->> fun vm (_, str, ()) -> vm.Push(Sexp.Num(float str.Length)) ]

    register builtin "str-concat"
    <| [ ("str-concat", Rest(Str "strs"))
         ->> fun vm (_, strs) -> vm.Push(Sexp.Str(ByteString.concat strs)) ]

    register builtin "substr"
    <| [ ("substr", Str "str", Num "index", Num "bytesize", ())
         ->> fun vm (_, str, index, bytesize, ()) ->
             let substr =
                 try
                     str.[int index .. int index + int bytesize - 1]
                 with :? IndexOutOfRangeException ->
                     raise (EvaluationErrorException "Index out of range")

             vm.Push(Sexp.Str substr) ]

    register builtin "sym->str"
    <| [ ("sym->str", Sym "symbol", ())
         ->> fun vm (_, sym, ()) -> vm.Push(Sexp.Str(ByteString.encode sym)) ]

    register builtin "num->str"
    <| [ ("num->str", Num "number", ())
         ->> fun vm (_, num, ()) -> vm.Push(Sexp.Str(ByteString.encode (num.ToString()))) ]

    register builtin "str->num"
    <| [ ("str->num", Str "string", ())
         ->> fun vm (_, str, ()) ->
             let num =
                 match Double.TryParse(ByteString.decode str) with
                 | true, num -> Sexp.Num num
                 | false, _ -> Sexp.Nil

             vm.Push num ]

    register builtin "vec"
    <| [ ("vec", Rest "items")
         ->> fun vm (_, items) -> vm.Push(Sexp.Pure(Native.Vec(List.toArray items))) ]

    register builtin "vec-make"
    <| [ ("vec-make", Num "length", "init", ())
         ->> fun vm (_, length, init, ()) ->
             let a = Array.create (int length) init
             vm.Push(Sexp.Pure(Native.Vec a)) ]

    register builtin "vec-ref"
    <| [ ("vec-ref", Vec "vec", Num "index", ())
         ->> fun vm (_, vec, index, ()) ->
             let s =
                 try
                     vec.[int index]
                 with :? IndexOutOfRangeException ->
                     Sexp.Nil

             vm.Push s ]

    register builtin "vec-length"
    <| [ ("vec-length", Vec "vec", ())
         ->> fun vm (_, vec, ()) -> vm.Push(Sexp.Num(float vec.Length)) ]

    register builtin "vec-set!"
    <| [ ("vec-set!", Vec "vec", Num "index", "item", ())
         ->> fun vm (_, vec, index, item, ()) ->
             try
                 vec.[int index] <- item
             with :? IndexOutOfRangeException ->
                 raise (EvaluationErrorException "Index out of range")

             vm.Push Sexp.Nil ]

    register builtin "vec-copy!"
    <| [ ("vec-set!", Vec "dest", Num "dest-start", (Vec "src", Num "src-start", Num "length", ()))
         ->> fun vm (_, dest, destStart, (src, srcStart, length, ())) ->
             try
                 Array.Copy(src, int srcStart, dest, int destStart, int length)
             with :? ArgumentException ->
                 raise (EvaluationErrorException "Index out of range")

             vm.Push Sexp.Nil ]

    register builtin "open"
    <| [ ("open", Str "filepath", Str "mode", ())
         ->> fun vm (_, filepath, mode, ()) ->
             fun () ->
                 let mode =
                     match ByteString.decode mode with
                     | "w" -> FileMode.Create
                     | "r" -> FileMode.Open
                     | mode -> raise (EvaluationErrorException("Unsupported mode for open: " + mode))

                 let stream = File.Open(ByteString.decode filepath, mode)
                 Sexp.Pure(Native.Port stream)
             |> tryIO
             |> vm.Push ]

    register builtin "close"
    <| [ ("close", Port "port", ())
         ->> fun vm (_, port, ()) ->
             fun () ->
                 port.Dispose()
                 Sexp.Nil
             |> tryIO
             |> vm.Push ]

    register builtin "stdin"
    <| [ ("stdin", ())
         ->> fun vm (_, ()) -> vm.Push(Sexp.Pure(Native.Port(Console.OpenStandardInput()))) ]

    register builtin "stdout"
    <| [ ("stdout", ())
         ->> fun vm (_, ()) -> vm.Push(Sexp.Pure(Native.Port(Console.OpenStandardOutput()))) ]

    register builtin "stderr"
    <| [ ("stderr", ())
         ->> fun vm (_, ()) -> vm.Push(Sexp.Pure(Native.Port(Console.OpenStandardError()))) ]

    register builtin "read-byte"
    <| [ ("read-byte", Port "port", ())
         ->> fun vm (_, port, ()) ->
             fun () ->
                 let byte = port.ReadByte()
                 if byte = -1 then Sexp.Sym "eof" else Sexp.Num(float byte)
             |> tryIO
             |> vm.Push ]

    register builtin "read-str"
    <| [ ("read-str", Num "bytesize", Port "port", ())
         ->> fun vm (_, bytesize, port, ()) ->
             fun () ->
                 let buf = Array.create (int bytesize) 0uy
                 let read = port.Read(buf, 0, int bytesize)

                 if read = 0 then
                     Sexp.Sym "eof"
                 else
                     Sexp.Str(ByteString.Create(buf, 0, read))
             |> tryIO
             |> vm.Push ]

    register builtin "read-line"
    <| [ ("read-line", Port "port", ())
         ->> fun vm (_, port, ()) ->
             fun () ->
                 let buf = Collections.Generic.List()

                 let read () =
                     match port.ReadByte() with
                     | -1 -> false
                     | 10 ->
                         buf.Add(10uy)
                         false // LF
                     | c ->
                         buf.Add(byte c)
                         true

                 while read () do
                     ()

                 match Array.ofSeq buf with
                 | [||] -> Sexp.Sym "eof"
                 | line -> Sexp.Str(ByteString.Create(line, 0, line.Length - 1))
             |> tryIO
             |> vm.Push ]

    register builtin "write-byte"
    <| [ ("write-byte", Num "byte", Port "port", ())
         ->> fun vm (_, b, port, ()) ->
             fun () ->
                 port.WriteByte(byte b)
                 Sexp.Num 1.0
             |> tryIO
             |> vm.Push ]

    register builtin "write-str"
    <| [ ("write-str", Str "str", Port "port", ())
         ->> fun vm (_, str, port, ()) ->
             fun () ->
                 let seg = str.ArraySegment
                 port.Write(seg.Array, seg.Offset, seg.Count)
                 Sexp.Num(float str.Length)
             |> tryIO
             |> vm.Push ]

    register builtin "write-line"
    <| [ ("write-str", Str "str", Port "port", ())
         ->> fun vm (_, str, port, ()) ->
             fun () ->
                 let seg = str.ArraySegment
                 port.Write(seg.Array, seg.Offset, seg.Count)
                 port.WriteByte 10uy // LF
                 port.Flush()
                 Sexp.Num(float (str.Length + 1))
             |> tryIO
             |> vm.Push ]

    register builtin "flush"
    <| [ ("flush", Port "port", ())
         ->> fun vm (_, port, ()) ->
             fun () ->
                 port.Flush()
                 Sexp.Nil
             |> tryIO
             |> vm.Push ]

    register builtinArgs "args" ("args", args)

    register builtin "eval"
    <| [ ("eval", "expr", ())
         ->> fun vm (_, expr, ()) ->
             match vm.Context.Eval expr with
             | Ok v -> vm.Push(Sexp.Cons(Sexp.Bool true, v))
             | Error e -> vm.Push(Sexp.Cons(Sexp.Bool false, Sexp.Str(ByteString.encode e))) ]

    register builtin "macroexpand"
    <| [ ("macroexpand", "expr", ())
         ->> fun vm (_, expr, ()) ->
             match vm.Context.MacroExpand true expr with
             | Ok v -> vm.Push(Sexp.Cons(Sexp.Bool true, v))
             | Error e -> vm.Push(Sexp.Cons(Sexp.Bool false, Sexp.Str(ByteString.encode e))) ]

    register builtin "macroexpand-1"
    <| [ ("macroexpand-1", "expr", ())
         ->> fun vm (_, expr, ()) ->
             match vm.Context.MacroExpand false expr with
             | Ok v -> vm.Push(Sexp.Cons(Sexp.Bool true, v))
             | Error e -> vm.Push(Sexp.Cons(Sexp.Bool false, Sexp.Str(ByteString.encode e))) ]
