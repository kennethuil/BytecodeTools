namespace CodeInjector

module Injector =
    open System
    open System.Reflection
    open System.Linq.Expressions
    open Mono.Cecil
    open Mono.Cecil.Cil
    
    // An F# lambda can magically become a LINQ expression,
    // but only if it's a parameter of a static method.
    // So we run it through right here.
    // NOTE: Trying to overload Quote with an Expression<System.Func<'a,'b>>
    // fails because it can't decide between Action<'a> and <Func<'a,unit>>.
    type Expr = 
        static member Quote<'a>(e:Expression<System.Action<'a>>) = e

    let createLdarg (ilp:ILProcessor) i =
        match i with
        | 0 -> ilp.Create(OpCodes.Ldarg_0)
        | 1 -> ilp.Create(OpCodes.Ldarg_1)
        | 2 -> ilp.Create(OpCodes.Ldarg_2)
        | 3 -> ilp.Create(OpCodes.Ldarg_3)
        | x when x < 256 -> ilp.Create(OpCodes.Ldarg_S, byte i)
        | x -> ilp.Create(OpCodes.Ldarg, i)


    let getLoadTargetArg (ilp:ILProcessor) parameterPos (p:ParameterDefinition)  =
        // TODO: look at target argument attributes

        // in the absence of attributes, incoming parameter N should be passed to
        // target parameter N
        let ldarg = createLdarg ilp parameterPos
        [ldarg]

    let insertInstructionsBefore (ilp:ILProcessor) (insertBefore:Instruction) (instrs) =
        instrs |> Seq.iter (fun x -> (ilp.InsertBefore(insertBefore, x)))

    let patchMethodBegin (injectionTarget:MethodDefinition) (injectedCallTarget:MethodReference) =
        let body = injectionTarget.Body;
        let instructions = body.Instructions;
        let ilp = body.GetILProcessor();

        // TODO: Skip initial call to base constructor, if present.
        let first = instructions.[0]

        // NOTE: Target should be static, and accessible from method m.
        let targetParams = injectedCallTarget.Parameters;
        let targetParamLoads = targetParams |> Seq.mapi (getLoadTargetArg ilp)
        targetParamLoads |> Seq.concat |> insertInstructionsBefore ilp first

        // Call the injectedCallTarget method.
        let call = ilp.Create(OpCodes.Call, injectedCallTarget)
        ilp.InsertBefore(first, call)
        
        injectionTarget

    let getMethodRefFromLambda (importInto:ModuleDefinition)  (x:LambdaExpression) =
        let body = x.Body
        match body with
        | :? MethodCallExpression as c ->
            let methodInfo = c.Method
            importInto.Import methodInfo
        | _ -> failwith ("Not currently supporting this type of expression")
