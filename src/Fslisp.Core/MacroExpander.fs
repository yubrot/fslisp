namespace Fslisp.Core

type MacroExpander(context: IContext) =
    member self.Expand (recurse: bool) (expr: Value): Value =
        match expr with
        | Sexp.List (m :: args) ->
            match context.TopLevel.Refer m with
            | Some (Sexp.Pure (Native.Syntax syntax)) ->
                if recurse then
                    Sexp.List (syntax.MacroExpand self (m :: args))
                else
                    expr
            | Some (Sexp.Pure (Native.Macro closure)) ->
                let env = Env(Some closure.Env)
                match Pattern.bind closure.Pattern args with
                | Ok mapping ->
                    Map.iter env.Define mapping
                    let expr = VM.Execute context env closure.Body
                    if recurse then self.Expand true expr else expr
                | Error e ->
                    raise (EvaluationErrorException ("This macro " + e))
            | _ ->
                if recurse then self.ExpandChildren expr else expr
        | _ ->
            if recurse then self.ExpandChildren expr else expr

    member self.ExpandChildren (expr: Value): Value =
        match expr with
        | Sexp.Cons (a, b) ->
            Sexp.Cons (self.Expand true a, self.ExpandChildren b)
        | expr ->
            expr

    interface IMacroExpander with
        member self.Expand recurse expr = self.Expand recurse expr

    static member MacroExpand (context: IContext) (recurse: bool) (expr: Value): Value =
        MacroExpander(context).Expand recurse expr
