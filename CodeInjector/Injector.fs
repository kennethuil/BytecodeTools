namespace CodeInjector

module Injector =
    open System
    open System.Reflection
    open System.Linq.Expressions
    open Mono.Cecil
    open Mono.Cecil.Cil

    let getMethodRefFromLambda (importInto:ModuleDefinition) (x:LambdaExpression) =
        let body = x.Body
        match body with
        | :? MethodCallExpression as c ->
            let methodInfo = c.Method
            importInto.Import methodInfo
        | _ -> failwith ("Not currently supporting this type of expression")
    
    // An F# lambda can magically become a LINQ expression,
    // but only if it's a parameter of a static method.
    type Expr = 
        static member Quote<'a>(e:Expression<System.Action<'a>>) = e

        // Turn a simple lambda that calls a function into a MethodRef.
        // NOTE: If your lambda calls a generic method, the MethodRef will have all the
        // generic parameters filled in from whatever your lambda specifies (you can't have a
        // call to a generic method with open generic parameters, either in a lambda or anywhere else)
        // Also, the lambda is never executed directly, and parameters to the function call in it
        // are completely ignored.
        static member MethodRefFromLambda<'a>  ((x:Expression<Action<'a>>),(importInto:ModuleDefinition)) =
            getMethodRefFromLambda importInto x

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
        // TODO: handle generic arguments

        // in the absence of attributes, incoming parameter N should be passed to
        // target parameter N
        let ldarg = createLdarg ilp p.Index
        [ldarg]

    let insertInstructionsBefore (ilp:ILProcessor) (insertBefore:Instruction) (instrs) =
        instrs |> Seq.iter (fun x -> (ilp.InsertBefore(insertBefore, x)))

    let patchBefore (ilp:ILProcessor) (instrs) (insertBefore:Instruction) = insertInstructionsBefore ilp insertBefore instrs

    
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

    let patchMethodBypass (injectionTarget:MethodDefinition) (injectedCallTarget:MethodReference) =
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

        // Return whatever the injected call returned
        ilp.InsertBefore(first, ilp.Create(OpCodes.Ret))        
        injectionTarget

    // This breaks if someone ever defines and uses a custom type named "System.Void".
    // So... hopefully no one goes and does that.
    let isVoidType (t:TypeReference) = t.FullName = "System.Void"
        

    let patchMethodReturn (injectionTarget:MethodDefinition) (injectedCallTarget:MethodReference) =
        let body = injectionTarget.Body;
        let instructions = body.Instructions;
        let ilp = body.GetILProcessor();

        let returnType = injectionTarget.ReturnType


        // Build the list of instructions to put before each return
        let patch =
            if (isVoidType returnType) then
                // Just pass through the parameters and call the patch method
                let targetParams = injectedCallTarget.Parameters;
                let targetParamLoads = targetParams |> Seq.mapi (getLoadTargetArg ilp) |> Seq.concat
                Seq.append targetParamLoads (Seq.singleton (ilp.Create(OpCodes.Call, injectedCallTarget)))
            else
                // Create a variable to hold the return value
                let returnVariable = new VariableDefinition(returnType)
                body.Variables.Add(returnVariable)

                // Snippets to save the return value, call the patch method with incoming parameters
                // and return value
                let saveReturnValue = Seq.singleton (ilp.Create(OpCodes.Stloc, returnVariable))
                let targetParams = injectionTarget.Parameters;
                let targetParamLoads = targetParams |> Seq.mapi (getLoadTargetArg ilp) |> Seq.concat
                // NOTE: Right now we're requiring the patch method to accept the return value as its last parameter.
                let loadReturnValue = Seq.singleton (ilp.Create(OpCodes.Ldloc, returnVariable))
                let callPatchMethod = Seq.singleton (ilp.Create(OpCodes.Call, injectedCallTarget))
                let reloadReturnValue =
                    if (isVoidType injectedCallTarget.ReturnType) then Seq.singleton (ilp.Create(OpCodes.Ldloc, returnVariable))
                    else Seq.empty<Instruction>

                Seq.concat [|saveReturnValue; targetParamLoads; loadReturnValue; callPatchMethod; reloadReturnValue|]

        // Find the return instructions
        // TODO: handle tail calls
        // TODO: What about the "jmp" instruction?
        let retInstructions = instructions |> Seq.filter(fun x->x.OpCode = OpCodes.Ret) |> Array.ofSeq

        retInstructions |> Seq.iter (patchBefore ilp patch)

        injectionTarget
        


