namespace Fslisp.Core

exception EvaluationErrorException of string

exception InternalErrorException of string

type VM(env: Env<Value>, code: Code<Value>) =
    let mutable stack = []
    let mutable code = code
    let mutable env = env
    let mutable dump = []

    member _.Push (value: Value) =
        stack <- value :: stack

    member _.Pop(): Value =
        match stack with
        | head :: rest ->
            stack <- rest
            head
        | _ ->
            raise (InternalErrorException "Inconsistent stack")

    member _.Enter (nextEnv: Env<Value>) (nextCode: Code<Value>) =
        match code.Instructions with
        | [Inst.Leave] -> () // tailcall: skip this frame
        | _ -> dump <- (env, code) :: dump
        env <- nextEnv
        code <- nextCode

    member _.Leave() =
        match dump with
        | (lastEnv, lastCode) :: rest ->
            env <- lastEnv
            code <- lastCode
            dump <- rest
        | _ ->
            raise (InternalErrorException "Inconsistent dump")

    member self.Apply (f: Value) (args: Value list) =
        match f with
        | Sexp.Pure (Native.Fun (fenv, fpat, fcode)) ->
            let env = Env(Some fenv)
            match Pattern.bind fpat args with
            | Ok mapping ->
                Map.iter env.Define mapping
                self.Enter env fcode
            | Error e ->
                raise (InternalErrorException ("This function " + e))
        | _ ->
            raise (EvaluationErrorException "Cannot call: ")

    member self.RunInst (inst: Inst<Value>) =
        match inst with
        | Inst.Ldc constant ->
            self.Push constant
        | Inst.Ldv variable ->
            self.Push (env.Get variable)
        | Inst.Ldf (pattern, code) ->
            self.Push (Sexp.Pure (Native.Fun (env, pattern, code)))
        | Inst.Ldm (pattern, code) ->
            self.Push (Sexp.Pure (Native.Macro (env, pattern, code)))
        | Inst.Ldb name ->
            raise (System.NotImplementedException "ldb")
        | Inst.Sel (a, b) ->
            let branch = if self.Pop() |> Sexp.test then a else b
            self.Enter (Env(Some env)) branch
        | Inst.App argc ->
            let mutable args = []
            for _ = 1 to argc do args <- self.Pop() :: args
            let f = self.Pop()
            self.Apply f args
        | Inst.Leave ->
            self.Leave()
        | Inst.Pop ->
            self.Pop() |> ignore
        | Inst.Def name ->
            let v = self.Pop()
            env.Define name v
        | Inst.Set name ->
            let v = self.Pop()
            env.Set name v

    member self.Run(): Value =
        match code.Next() with
        | Some (inst, rest) ->
            code <- rest
            self.RunInst inst
            self.Run()
        | None ->
            self.Pop()

    static member Execute (env: Env<Value>) (code: Code<Value>): Value =
        VM(env, code).Run()
