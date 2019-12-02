module Fslisp.Core.Parser

open FParsec

[<RequireQualifiedAccess>]
module private Token =
    let ambient =
        choice [
            spaces1
            skipChar ';' >>. skipManySatisfy (fun c -> c <> '\n')
        ]
        |> skipMany

    let lex p = p .>> ambient

    let LParen = lex (skipChar '(')
    let RParen = lex (skipChar ')')
    let LBrack = lex (skipChar '[')
    let RBrack = lex (skipChar ']')
    let Dot = lex (skipChar '.')
    let True = lex (skipString "#t")
    let False = lex (skipString "#f")
    let Quote = lex (skipChar '\'')
    let Quasiquote = lex (skipChar '`')
    let Unquote = lex (skipChar ',')
    let UnquoteSplicing = lex (skipString ",@")
    let Num = lex pfloat <?> "number"
    let Sym =
        let isSpecial = isAnyOf "!$%&*+-/:<=>?@^_~"
        let f1 c = isLetter c || isSpecial c
        let f c = isLetter c || isDigit c || isSpecial c
        lex (many1Satisfy2L f1 f "symbol")
    let Str =
        let normal = manySatisfy (fun c -> c <> '\\' && c <> '"')
        let escaped = skipChar '\\' >>. (anyOf "\\nrt\"" |>> function
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | c -> string c)
        lex (between (pstring "\"") (pstring "\"") (stringsSepBy normal escaped))

[<RequireQualifiedAccess>]
module private Grammar =
    let S, SRef = createParserForwardedToRef<Sexp<unit>, unit>()

    let SInner =
        pipe2 (many1 S) (opt (Token.Dot >>. S)) (fun ss t ->
            Sexp.ListLike ss (Option.defaultValue Sexp.Nil t)
        )
        <|>% Sexp.Nil

    do SRef := choice [
        Token.LParen >>. SInner .>> Token.RParen
        Token.LBrack >>. SInner .>> Token.RBrack
        Token.Quote >>. S |>> Sexp.Quote
        Token.Quasiquote >>. S |>> Sexp.Quasiquote
        Token.UnquoteSplicing >>. S |>> Sexp.UnquoteSplicing
        Token.Unquote >>. S |>> Sexp.Unquote
        Token.Num |>> Sexp.Num
        Token.Sym |>> Sexp.Sym
        Token.Str |>> Sexp.Str
        Token.True >>% Sexp.Bool true
        Token.False >>% Sexp.Bool false
    ]

type Parser<'T> = Parser<'T, unit>

let sexp (): Parser<Sexp<'T>> =
    Grammar.S |>> Sexp.map (fun _ -> failwith "pure")

let program (): Parser<Sexp<'T> list> =
    many (sexp ())

let parse (p: unit -> Parser<'T>) name input (start: int option): Result<'T * int, string> =
    match start with
    | None -> 0, runParserOnString (Token.ambient >>. (p ())) () name input
    | Some s -> s, runParserOnSubstring (p ()) () name input s (input.Length - s)
    |> function
        | i, Success (r, _, p) -> Result<_, _>.Ok (r, i + int p.Index)
        | _, Failure (e, _, _) -> Result<_, _>.Error e

let parseToEnd (p: unit -> Parser<'T>) name input: Result<'T, string> =
    runParserOnString (Token.ambient >>. (p ()) .>> eof) () name input
    |> function
        | Success (r, _, _) -> Result<_, _>.Ok r
        | Failure (e, _, _) -> Result<_, _>.Error e
