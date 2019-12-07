namespace Fslisp.Core

type MacroExpander(builtinTable: BuiltinTable, macroExpanderEnv: Env<Value>) =
    member self.Expand (recurse: bool) (expr: Value): Value =
        match expr with
        | Sexp.List (m :: args) ->
            match macroExpanderEnv.Refer m with
            | Some (Sexp.Pure (Native.Syntax syntax)) ->
                if recurse then
                    Sexp.List (syntax.MacroExpand self (m :: args))
                else
                    expr
            | Some (Sexp.Pure (Native.Macro (menv, mpat, mcode))) ->
                let env = Env(Some menv)
                match Pattern.bind mpat args with
                | Ok mapping ->
                    Map.iter env.Define mapping
                    let expr = VM.Execute builtinTable env mcode
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

    static member MacroExpand (builtinTable: BuiltinTable) (macroExpanderEnv: Env<Value>) (recurse: bool) (expr: Value): Value =
        MacroExpander(builtinTable, macroExpanderEnv).Expand recurse expr
