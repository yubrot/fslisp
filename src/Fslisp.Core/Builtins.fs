module Fslisp.Core.Builtins

open Signature

let builtin arms =
    { new IBuiltin with
        member _.Run vm expr =
            match handle arms vm expr with
            | Ok () -> ()
            | Error e -> raise (EvaluationErrorException e)
    }

let builtinGensym (name: string) =
    let mutable id = 0
    builtin [
        (name, ()) ->>
            fun vm (_, ()) ->
                id <- id + 1
                vm.Push(Sexp.Sym (sprintf "#sym.%d" id))
    ]

let builtinTest (name: string) f = builtin [
    (name, "expr", ()) ->>
        fun vm (_, expr, ()) -> vm.Push (Sexp.Bool (f expr))
]

let builtinCompare (name: string) numOp strOp =
    let rec go compare x xs =
        match xs with
        | [] -> true
        | y :: xs -> compare x y && go compare y xs
    builtin [
        (name, ()) ->>
            fun vm (_, ()) -> vm.Push (Sexp.Bool true)
        (name, Num "num", Rest (Num "nums")) ->>
            fun vm (_, a, bs) -> vm.Push (Sexp.Bool (go numOp a bs))
        (name, Str "str", Rest (Str "strs")) ->>
            fun vm (_, a, bs) -> vm.Push (Sexp.Bool (go strOp a bs))
    ]

let builtinCont (cont: Cont) = builtin [
    ("continuation", ()) ->>
        fun vm (_, ()) ->
            vm.ApplyCont cont
            vm.Push Sexp.Nil
    ("continuation", "x", ()) ->>
        fun vm (_, x, ()) ->
            vm.ApplyCont cont
            vm.Push x
    ("continuation", Rest "xs") ->>
        fun vm (_, _) ->
            raise (EvaluationErrorException "Multiple values are not implemented")
]

let table: BuiltinTable = Map.ofList [
    "cons", builtin [
        ("cons", "car", "cdr", ()) ->>
            fun vm (_, a, b, ()) -> vm.Push (Sexp.Cons (a, b))
    ]

    "exit", builtin [
        ("exit", ()) ->>
            fun vm (_, ()) -> exit 0
        ("exit", Num "exitcode", ()) ->>
            fun vm (_, c, ()) -> exit (int c)
    ]
    "error", builtin [
        ("error", ()) ->>
            fun vm (_, ()) -> raise (EvaluationErrorException "error called")
        ("error", Str "msg", ()) ->>
            fun vm (_, msg, ()) -> raise (EvaluationErrorException msg)
    ]

    "gensym", builtinGensym "gensym"

    "car", builtin [
        ("car", Cons ("a", "b"), ()) ->>
            fun vm (_, (a, _), ()) -> vm.Push a
    ]
    "cdr", builtin [
        ("cdr", Cons ("a", "b"), ()) ->>
            fun vm (_, (_, b), ()) -> vm.Push b
    ]

    "apply", builtin [
        ("apply", "f", List "argument-list", ()) ->>
            fun vm (_, f, args, ()) -> vm.Apply f args
    ]

    "num?", builtinTest "num?" (function Sexp.Num _ -> true | _ -> false)
    "sym?", builtinTest "sym?" (function Sexp.Sym _ -> true | _ -> false)
    "str?", builtinTest "str?" (function Sexp.Str _ -> true | _ -> false)
    "cons?", builtinTest "cons?" (function Sexp.Cons _ -> true | _ -> false)
    "nil?", builtinTest "nil?" (function Sexp.Nil -> true | _ -> false)
    "bool?", builtinTest "bool?" (function Sexp.Bool _ -> true | _ -> false)
    "proc?", builtinTest "proc?" (function Sexp.Pure (Native.Builtin _ | Native.Fun _) -> true | _ -> false)
    "meta?", builtinTest "proc?" (function Sexp.Pure (Native.Syntax _ | Native.Macro _) -> true | _ -> false)
    "port?", builtinTest "proc?" (fun _ -> false)
    "vec?", builtinTest "vec?" (fun _ -> false)

    "+", builtin [
        ("+", Rest (Num "nums")) ->>
            fun vm (_, nums) -> vm.Push (Sexp.Num (Seq.fold (+) 0.0 nums))
    ]
    "-", builtin [
        ("-", Num "num", ()) ->>
            fun vm (_, num, ()) -> vm.Push (Sexp.Num (- num))
        ("-", Num "num", Rest (Num "nums")) ->>
            fun vm (_, a, bs) -> vm.Push (Sexp.Num (Seq.fold (-) a bs))
    ]
    "*", builtin [
        ("*", Rest (Num "nums")) ->>
            fun vm (_, nums) -> vm.Push (Sexp.Num (Seq.fold ( *) 1.0 nums))
    ]
    "/", builtin [
        ("/", Num "num", ()) ->>
            fun vm (_, num, ()) -> vm.Push (Sexp.Num (1.0 / num))
        ("/", Num "num", Rest (Num "nums")) ->>
            fun vm (_, a, bs) -> vm.Push (Sexp.Num (Seq.fold (/) a bs))
    ]
    "%", builtin [
        ("%", Num "num", Rest (Num "nums")) ->>
            fun vm (_, a, bs) -> vm.Push (Sexp.Num (Seq.fold (%) a bs))
    ]

    "=", builtin [
        ("=", ()) ->>
            fun vm (_, ()) -> vm.Push (Sexp.Bool true)
        ("=", "x", Rest "xs") ->>
            fun vm (_, x, xs) -> vm.Push (Sexp.Bool (Seq.forall (Sexp.equal x) xs))
    ]

    "<", builtinCompare "<" (<) (<)
    ">", builtinCompare ">" (>) (>)
    "<=", builtinCompare "<=" (<=) (<=)
    ">=", builtinCompare ">=" (>=) (>=)

    "call/cc", builtin [
        ("call/cc", "f", ()) ->>
            fun vm (_, f, ()) ->
                let cont = vm.CaptureCont()
                vm.Apply f [Sexp.Pure (Native.Builtin (builtinCont cont))]
    ]
    "never", builtin [
        ("never", "f", Rest "args") ->>
            fun vm (_, f, args) -> vm.ApplyNever f args
    ]
]
