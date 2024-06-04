namespace Fslisp.Core

[<Struct>]
type Pattern =
    { Fixed: string list
      Rest: string option }

    override self.ToString() =
        let f = self.Fixed |> List.map Sexp.Sym

        let r =
            match self.Rest with
            | Some s -> Sexp.Sym s
            | None -> Sexp.Nil

        (Sexp.ListLike f r).ToString()

[<RequireQualifiedAccess>]
module Pattern =
    let rec build (s: Sexp<'T>) : Result<Pattern, Sexp<'T>> =
        match s with
        | Sexp.Sym sym -> Ok { Fixed = []; Rest = Some sym }
        | Sexp.Nil -> Ok { Fixed = []; Rest = None }
        | Sexp.Cons(Sexp.Sym a, b) -> Result.map (fun p -> { p with Fixed = a :: p.Fixed }) (build b)
        | Sexp.Cons(a, _) -> Error a
        | s -> Error s

    let rec bind (pattern: Pattern) (args: Sexp<'T> list) : Result<Map<string, Sexp<'T>>, string> =
        match pattern.Fixed, pattern.Rest, args with
        | p :: ps, rest, a :: args -> Result.map (Map.add p a) (bind { Fixed = ps; Rest = rest } args)
        | [], None, [] -> Ok Map.empty
        | [], Some rest, args -> Ok(Map.ofArray [| rest, Sexp.List args |])
        | _ :: _, Some _, [] -> Error(sprintf "takes at least %d arguments" (List.length pattern.Fixed))
        | _ :: _, None, []
        | _, None, _ :: _ -> Error(sprintf "takes %d arguments" (List.length pattern.Fixed))
