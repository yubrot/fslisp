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

[<RequireQualifiedAccess>]
module Sexp =
    let rec map mapping s =
        match s with
        | Sexp.Num n -> Sexp.Num n
        | Sexp.Sym s -> Sexp.Sym s
        | Sexp.Str s -> Sexp.Str s
        | Sexp.Cons (a, b) -> Sexp.Cons (map mapping a, map mapping b)
        | Sexp.Nil -> Sexp.Nil
        | Sexp.Bool b -> Sexp.Bool b
        | Sexp.Pure v -> Sexp.Pure (mapping v)

    let test s =
        match s with
        | Sexp.Bool b -> b
        | _ -> true

    let ListLike ss t =
        List.foldBack (fun a b -> Sexp.Cons(a, b)) ss t

    let List ss =
        ListLike ss Sexp.Nil

    let rec (|List|_|) s =
        match s with
        | Sexp.Nil -> Some []
        | Sexp.Cons(a, List bs) -> Some (a :: bs)
        | _ -> None

    let Quote s =
        List [Sexp.Sym "quote"; s]

    let (|Quote|_|) s =
        match s with List [Sexp.Sym "quote"; s] -> Some s | _ -> None

    let Quasiquote s =
        List [Sexp.Sym "quasiquote"; s]

    let (|Quasiquote|_|) s =
        match s with List [Sexp.Sym "quasiquote"; s] -> Some s | _ -> None

    let Unquote s =
        List [Sexp.Sym "unquote"; s]

    let (|Unquote|_|) s =
        match s with List [Sexp.Sym "unquote"; s] -> Some s | _ -> None

    let UnquoteSplicing s =
        List [Sexp.Sym "unquote-splicing"; s]

    let (|UnquoteSplicing|_|) s =
        match s with List [Sexp.Sym "unquote-splicing"; s] -> Some s | _ -> None

    let prettyPrint prettyPrintPure s =
        let escape =
            String.collect (function
                | '\\' -> "\\\\"
                | '\t' -> "\\t"
                | '\n' -> "\\n"
                | '"' -> "\""
                | c -> c.ToString()
            )
        let rec sexp s =
            match s with
            | Sexp.Num n ->
                if n % 1.0 = 0.0 then
                    sprintf "%d" (int64 n)
                else
                    sprintf "%A" n
            | Sexp.Sym s -> s
            | Sexp.Str s -> sprintf "\"%s\"" (escape s)
            | Quote s -> sprintf "'%s" (sexp s)
            | Quasiquote s -> sprintf "`%s" (sexp s)
            | Unquote s -> sprintf ",%s" (sexp s)
            | UnquoteSplicing s -> sprintf ",@%s" (sexp s)
            | Sexp.Cons (a, b) -> sprintf "(%s)" (cons a b)
            | Sexp.Nil -> "()"
            | Sexp.Bool true -> "#t"
            | Sexp.Bool false -> "#f"
            | Sexp.Pure p -> prettyPrintPure p
        and cons a b =
            match b with
            | Sexp.Nil -> sexp a
            | Sexp.Cons (b, c) -> sprintf "%s %s" (sexp a) (cons b c)
            | b -> sprintf "%s . %s" (sexp a) (sexp b)
        sexp s
