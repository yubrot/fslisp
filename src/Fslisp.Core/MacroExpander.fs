namespace Fslisp.Core

type MacroExpander(env: Env<Value>) =
    member self.Expand recurse expr =
        match expr with
        | Sexp.List (m :: args) ->
            match env.Refer m with
            | Some (Sexp.Pure (Syntax syntax)) ->
                if recurse then
                    Sexp.List (m :: syntax.MacroExpand self args)
                else
                    expr
            | Some (Sexp.Pure (Macro (menv, mpat, mcode))) ->
                let env = Env(Some menv)
                match Pattern.bind mpat args with
                | Ok mapping ->
                    Map.iter env.Define mapping
                    let expr = VM.Execute env mcode
                    if recurse then self.Expand true expr else expr
                | Error e ->
                    raise (EvaluationErrorException ("This macro " + e))
            | _ ->
                if recurse then self.ExpandChildren expr else expr
        | _ ->
            if recurse then self.ExpandChildren expr else expr

    member self.ExpandChildren expr =
        match expr with
        | Sexp.Cons (a, b) ->
            Sexp.Cons (self.Expand true a, self.ExpandChildren b)
        | expr ->
            expr

    interface IMacroExpander with
        member self.Expand recurse expr = self.Expand recurse expr

    static member MacroExpand env recurse expr =
        MacroExpander(env).Expand recurse expr
