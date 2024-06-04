module Fslisp.Core.Signature

type MatchFailedReason =
    | ArityMismatch // TODO: Expected [at least] X arguments but got Y
    | TypeMismatch // TODO: Expected X but got Y
    | InvalidPattern of string

    override self.ToString() =
        match self with
        | ArityMismatch -> "arity mismatch"
        | TypeMismatch -> "type mismatch"
        | InvalidPattern p -> "invalid pattern: " + p

exception MatchFailedException of MatchFailedReason

[<Struct>]
type Match = Match

[<Struct>]
type Placeholder = Placeholder

let inline private instance (a: ^a, b: ^b) =
    ((^a or ^b): (static member Instance: _ * _ -> ^n) a, b)

let inline match' (m: ^m) : ^n = instance (Match, m)
let inline placeholder (m: ^m) : ^n = instance (Placeholder, m)

let inline tryMatch m x =
    try
        Ok(match' m x)
    with MatchFailedException e ->
        Error(placeholder m, e)

[<Struct>]
type Num =
    | Num of string

    static member inline Instance(Match, Num _) =
        function
        | Sexp.Num n -> n
        | _ -> raise (MatchFailedException TypeMismatch)

    static member inline Instance(Placeholder, Num label) = Sexp.Sym label

[<Struct>]
type Sym =
    | Sym of string

    static member inline Instance(Match, Sym _) =
        function
        | Sexp.Sym s -> s
        | _ -> raise (MatchFailedException TypeMismatch)

    static member inline Instance(Placeholder, Sym label) = Sexp.Sym label

[<Struct>]
type Str =
    | Str of string

    static member inline Instance(Match, Str _) =
        function
        | Sexp.Str s -> s
        | _ -> raise (MatchFailedException TypeMismatch)

    static member inline Instance(Placeholder, Str label) = Sexp.Sym label

[<Struct>]
type Cons< ^a, ^b> =
    | Cons of ^a * ^b

    static member inline Instance(Match, Cons(a, b)) =
        function
        | Sexp.Cons(a', b') -> match' a a', match' b b'
        | _ -> raise (MatchFailedException TypeMismatch)

    static member inline Instance(Placeholder, Cons(a, b)) = Sexp.Cons(placeholder a, placeholder b)

[<Struct>]
type Nil = Nil
    with


        static member inline Instance(Match, Nil) =
            function
            | Sexp.Nil -> ()
            | _ -> raise (MatchFailedException TypeMismatch)

        static member inline Instance(Placeholder, Nil) = Sexp.Nil

[<Struct>]
type Bool =
    | Bool of string

    static member inline Instance(Match, Bool _) =
        function
        | Sexp.Bool b -> b
        | _ -> raise (MatchFailedException TypeMismatch)

    static member inline Instance(Placeholder, Bool label) = Sexp.Sym label

[<Struct>]
type List< ^a> =
    | List of ^a

    static member inline Instance(Match, List a) =
        function
        | Sexp.List ls -> List.map (match' a) ls
        | _ -> raise (MatchFailedException TypeMismatch)

    static member inline Instance(Placeholder, List a) =
        Sexp.List [ placeholder a; Sexp.Sym "..." ]

[<Struct>]
type Vec =
    | Vec of string

    static member inline Instance(Match, Vec _) =
        function
        | Sexp.Pure(Native.Vec a) -> a
        | _ -> raise (MatchFailedException TypeMismatch)

    static member inline Instance(Placeholder, Vec label) = Sexp.Sym label

[<Struct>]
type Pat =
    | Pat of string

    static member inline Instance(Match, Pat _) =
        fun x ->
            match Pattern.build x with
            | Ok p -> p
            | Error e -> raise (MatchFailedException(InvalidPattern(e.ToString())))

    static member inline Instance(Placeholder, Pat label) = Sexp.Sym label

module private Tuple =
    let inline match0 () =
        function
        | [] -> ()
        | _ -> raise (MatchFailedException ArityMismatch)

    let inline match2 (a: ^a, b: ^b) =
        function
        | a' :: b' -> match' a a', match' b b'
        | _ -> raise (MatchFailedException ArityMismatch)

    let inline match3 (a: ^a, b: ^b, c: ^c) =
        function
        | a' :: b' :: c' -> match' a a', match' b b', match' c c'
        | _ -> raise (MatchFailedException ArityMismatch)

    let inline match4 (a: ^a, b: ^b, c: ^c, d: ^d) =
        function
        | a' :: b' :: c' :: d' -> match' a a', match' b b', match' c c', match' d d'
        | _ -> raise (MatchFailedException ArityMismatch)

    let inline match5 (a: ^a, b: ^b, c: ^c, d: ^d, e: ^e) =
        function
        | a' :: b' :: c' :: d' :: e' -> match' a a', match' b b', match' c c', match' d d', match' e e'
        | _ -> raise (MatchFailedException ArityMismatch)

    let inline placeholder0 () = []
    let inline placeholder2 (a: ^a, b: ^b) = placeholder a :: placeholder b

    let inline placeholder3 (a: ^a, b: ^b, c: ^c) =
        placeholder a :: placeholder b :: placeholder c

    let inline placeholder4 (a: ^a, b: ^b, c: ^c, d: ^d) =
        placeholder a :: placeholder b :: placeholder c :: placeholder d

    let inline placeholder5 (a: ^a, b: ^b, c: ^c, d: ^d, e: ^e) =
        placeholder a
        :: placeholder b
        :: placeholder c
        :: placeholder d
        :: placeholder e

type Match with
    static member inline Instance(Match, _: string) = id
    static member inline Instance(Match, t) = Tuple.match0 t
    static member inline Instance(Match, t) = Tuple.match2 t
    static member inline Instance(Match, t) = Tuple.match3 t
    static member inline Instance(Match, t) = Tuple.match4 t
    static member inline Instance(Match, t) = Tuple.match5 t

type Placeholder with
    static member inline Instance(Placeholder, s) = Sexp.Sym s
    static member inline Instance(Placeholder, t) = Tuple.placeholder0 t
    static member inline Instance(Placeholder, t) = Tuple.placeholder2 t
    static member inline Instance(Placeholder, t) = Tuple.placeholder3 t
    static member inline Instance(Placeholder, t) = Tuple.placeholder4 t
    static member inline Instance(Placeholder, t) = Tuple.placeholder5 t

[<Struct>]
type Rest< ^a> =
    | Rest of ^a

    static member inline Instance(Match, Rest a) = List.map (match' a)
    static member inline Instance(Placeholder, Rest a) = [ placeholder a; Sexp.Sym "..." ]

let inline (->>) m body ctx x = Result.map (body ctx) (tryMatch m x)

let handle arms ctx x =
    let rec go arms errors =
        match arms with
        | arm :: arms ->
            match arm ctx x with
            | Ok a -> Ok a
            | Error e -> go arms (e :: errors)
        | [] ->
            let expect =
                errors
                |> Seq.map (fun (s, _) -> (Sexp.List s).ToString())
                |> String.concat " or "

            let reason =
                errors
                |> Seq.map (fun (_, e) -> e)
                |> Seq.sortDescending
                |> Seq.tryHead
                |> Option.map (fun e -> ": " + e.ToString())
                |> Option.defaultValue ""

            Error("expected " + expect + reason)

    go arms []
