namespace Fslisp.Core

open System.Collections.Generic

exception UndefinedVariableException of string

type Env<'T>(parent: Env<'T> option) =
    let table = Dictionary<string, 'T>()

    member self.Define key value =
        table.[key] <- value

    member self.Set key value =
        if table.ContainsKey(key) then
            table.[key] <- value
        else
            match parent with
            | Some env -> env.Set key value
            | None -> raise (UndefinedVariableException key)

    member self.Find key =
        match table.TryGetValue(key) with
        | true, value -> Some value
        | false, _ -> parent |> Option.bind (fun env -> env.Find key)

    member self.Get key =
        match self.Find key with
        | Some value -> value
        | None -> raise (UndefinedVariableException key)
