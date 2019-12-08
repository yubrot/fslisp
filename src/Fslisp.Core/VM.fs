namespace Fslisp.Core

type VM(context: IContext, env: Env<Value>, code: Code<Value>) =
    let mutable stack = []
    let mutable env = env
    let mutable code = code
    let mutable dump = []

    member _.Context = context

    member _.Push(value: Value) =
        stack <- value :: stack

    member _.Pop(): Value =
        match stack with
        | head :: rest ->
            stack <- rest
            head
        | _ ->
            raise (InternalErrorException "Inconsistent stack")

    member _.Enter(nextEnv: Env<Value>, nextCode: Code<Value>) =
        match code with
        | Code [Inst.Leave] -> () // tailcall: skip this frame
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

    member self.Apply(f: Value, args: Value list) =
        match f with
        | Sexp.Pure (Native.Builtin builtin) ->
            builtin.Run self (f :: args)
        | Sexp.Pure (Native.Fun closure) ->
            let env = Env(Some closure.Env)
            match Pattern.bind closure.Pattern args with
            | Ok mapping ->
                Map.iter env.Define mapping
                self.Enter(env, closure.Body)
            | Error e ->
                raise (InternalErrorException ("This function " + e))
        | _ ->
            raise (EvaluationErrorException "Cannot call: ")

    member self.ApplyNever(f: Value, args: Value list) =
        stack <- []
        code <- Code [Inst.Leave]
        dump <- []
        self.Apply(f, args)

    member _.ApplyCont(cont: Cont) =
        stack <- cont.Stack
        env <- cont.Env
        code <- cont.Code
        dump <- cont.Dump

    member _.CaptureCont(): Cont =
        { Stack = stack
          Env = env
          Code = code
          Dump = dump }

    member self.RunInst(inst: Inst<Value>) =
        match inst with
        | Inst.Ldc constant ->
            self.Push constant
        | Inst.Ldv variable ->
            self.Push (env.Get variable)
        | Inst.Ldf (pattern, body) ->
            self.Push (Sexp.Pure (Native.Fun { Env = env; Pattern = pattern; Body = body }))
        | Inst.Ldm (pattern, body) ->
            self.Push (Sexp.Pure (Native.Macro { Env = env; Pattern = pattern; Body = body }))
        | Inst.Ldb name ->
            match context.Builtins.Get name with
            | Some builtin ->
                self.Push (Sexp.Pure (Native.Builtin builtin))
            | None ->
                raise (EvaluationErrorException ("Unsupported builtin: " + name))
        | Inst.Sel (a, b) ->
            let branch = if self.Pop() |> Sexp.test then a else b
            self.Enter(Env(Some env), branch)
        | Inst.App argc ->
            let mutable args = []
            for _ = 1 to argc do args <- self.Pop() :: args
            let f = self.Pop()
            self.Apply(f, args)
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

    interface IVM with
        member self.Push(value) = self.Push(value)
        member self.Apply(f, args) = self.Apply(f, args)
        member self.ApplyNever(f, args) = self.ApplyNever(f, args)
        member self.ApplyCont(cont) = self.ApplyCont(cont)
        member self.CaptureCont() = self.CaptureCont()
        member self.Context = self.Context

    static member Execute (context: IContext) (env: Env<Value>) (code: Code<Value>): Value =
        VM(context, env, code).Run()
