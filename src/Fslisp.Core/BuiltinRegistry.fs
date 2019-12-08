namespace Fslisp.Core

open System.Collections.Generic

type BuiltinRegistry() =
    let table = Dictionary<string, IBuiltin>()

    member _.Register (name: string) (builtin: IBuiltin) =
        table.[name] <- builtin

    member _.Get (name: string) =
        match table.TryGetValue name with
        | true, builtin -> Some builtin
        | false, _ -> None

    interface IBuiltinRegistry with
        member self.Get name = self.Get name
