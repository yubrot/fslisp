namespace Fslisp.Core

[<RequireQualifiedAccess>]
type Sexp<'T> =
    | Num of float
    | Sym of string
    | Str of string
    | Cons of Sexp<'T> * Sexp<'T>
    | Nil
    | Bool of bool
    | Pure of 'T

    override self.ToString() =
        let escape =
            String.collect (function
                | '\\' -> "\\\\"
                | '\t' -> "\\t"
                | '\n' -> "\\n"
                | '"' -> "\\\""
                | c -> c.ToString())

        let rec cons a b =
            match b with
            | Nil -> a.ToString()
            | Cons(b, c) -> sprintf "%s %s" (a.ToString()) (cons b c)
            | b -> sprintf "%s . %s" (a.ToString()) (b.ToString())

        match self with
        | Num n ->
            if n % 1.0 = 0.0 then
                sprintf "%d" (int64 n)
            else
                sprintf "%A" n
        | Sym s -> s
        | Str s -> sprintf "\"%s\"" (escape s)
        // FIXME: I want to use active patterns here but I couldn't figure
        //        out a good way (it seems impossible?)
        | Cons(Sym "quote", Cons(s, Nil)) -> "'" + s.ToString()
        | Cons(Sym "quasiquote", Cons(s, Nil)) -> "`" + s.ToString()
        | Cons(Sym "unquote", Cons(s, Nil)) -> "," + s.ToString()
        | Cons(Sym "unquote-splicing", Cons(s, Nil)) -> ",@" + s.ToString()
        | Cons(a, b) -> sprintf "(%s)" (cons a b)
        | Nil -> "()"
        | Bool true -> "#t"
        | Bool false -> "#f"
        | Pure p -> p.ToString()

[<RequireQualifiedAccess>]
module Sexp =
    let rec map mapping s =
        match s with
        | Sexp.Num n -> Sexp.Num n
        | Sexp.Sym s -> Sexp.Sym s
        | Sexp.Str s -> Sexp.Str s
        | Sexp.Cons(a, b) -> Sexp.Cons(map mapping a, map mapping b)
        | Sexp.Nil -> Sexp.Nil
        | Sexp.Bool b -> Sexp.Bool b
        | Sexp.Pure v -> Sexp.Pure(mapping v)

    let test s =
        match s with
        | Sexp.Bool b -> b
        | _ -> true

    let ListLike ss t =
        List.foldBack (fun a b -> Sexp.Cons(a, b)) ss t

    let List ss = ListLike ss Sexp.Nil

    let rec (|List|_|) s =
        match s with
        | Sexp.Nil -> Some []
        | Sexp.Cons(a, List bs) -> Some(a :: bs)
        | _ -> None

    let Quote s = List [ Sexp.Sym "quote"; s ]

    let (|Quote|_|) s =
        match s with
        | List [ Sexp.Sym "quote"; s ] -> Some s
        | _ -> None

    let Quasiquote s = List [ Sexp.Sym "quasiquote"; s ]

    let (|Quasiquote|_|) s =
        match s with
        | List [ Sexp.Sym "quasiquote"; s ] -> Some s
        | _ -> None

    let Unquote s = List [ Sexp.Sym "unquote"; s ]

    let (|Unquote|_|) s =
        match s with
        | List [ Sexp.Sym "unquote"; s ] -> Some s
        | _ -> None

    let UnquoteSplicing s = List [ Sexp.Sym "unquote-splicing"; s ]

    let (|UnquoteSplicing|_|) s =
        match s with
        | List [ Sexp.Sym "unquote-splicing"; s ] -> Some s
        | _ -> None

    let rec equal a b =
        match a, b with
        | Sexp.Num a, Sexp.Num b -> a = b
        | Sexp.Sym a, Sexp.Sym b -> a = b
        | Sexp.Str a, Sexp.Str b -> a = b
        | Sexp.Cons(a, a2), Sexp.Cons(b, b2) -> equal a b && equal a2 b2
        | Sexp.Nil, Sexp.Nil -> true
        | Sexp.Bool a, Sexp.Bool b -> a = b
        | _, _ -> false

    let isNum s =
        match s with
        | Sexp.Num _ -> true
        | _ -> false

    let isSym s =
        match s with
        | Sexp.Sym _ -> true
        | _ -> false

    let isStr s =
        match s with
        | Sexp.Str _ -> true
        | _ -> false

    let isCons s =
        match s with
        | Sexp.Cons _ -> true
        | _ -> false

    let isNil s =
        match s with
        | Sexp.Nil -> true
        | _ -> false

    let isBool s =
        match s with
        | Sexp.Bool _ -> true
        | _ -> false

    let isPure f s =
        match s with
        | Sexp.Pure p -> f p
        | _ -> false
