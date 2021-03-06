﻿namespace BytecodeTools.Tests

open NUnit.Framework
open System
open System.IO
open System.Reflection
open Mono.Cecil
open CodeInjector.Injector
open CodeInjector.Attributes
open BytecodeTools.Tests.DomainHelper

module InjectTests =
    type ExampleTracer() =
        static let mutable messages = []

        static member WriteMessage (message:string) =
            messages <- message :: messages
        static member GetMessages() = messages

    let injectionTarget1 x y =
        x + y

    let voidInjectionTarget x y =
        ()

    let genericInjectionTarget x y =
        [|x;y|]

    let injectedCallTarget (x:int) (y:int) =
        ExampleTracer.WriteMessage("x = " + x.ToString() + ", y = " + y.ToString())

    let injectedReplacement (x:int) (y:int) =
        ExampleTracer.WriteMessage("Injected replacement: x = " + x.ToString() + ", y = " + y.ToString())
        42

    let genericInjectedCallTarget x y =
        let message = sprintf "x = %A, y = %A" x y
        ExampleTracer.WriteMessage(message)

    let injectedCallTargetForReturn (x:int) (y:int) (result:int) =
        let message = sprintf "return value = %A" result
        ExampleTracer.WriteMessage(message)

    let injectedCallTargetForReturnReplacement (x:int) (y:int) (result:int) =
        let message = sprintf "return value = %A" result
        ExampleTracer.WriteMessage(message)
        42


    // Need a class inheriting MarshalByRefObject to do cross-domain stuff
    type CrossDomainMethods() = 
        inherit MarshalByRefObject()

        member this.RunInjectionTarget1 x y =
            let z = injectionTarget1 x y
            (z, ExampleTracer.GetMessages())

        member this.InjectionTarget2 x y =
            x * y

        member this.RunInjectionTarget2 x y =
            let z = this.InjectionTarget2 x y
            (z, ExampleTracer.GetMessages())

        member this.RunGenericInjectionTarget a b =
            let z = genericInjectionTarget a b
            (z, ExampleTracer.GetMessages())


    let injectedCallTargetWithThis (this:CrossDomainMethods) (x:int) (y:int) =
        ExampleTracer.WriteMessage("x = "+ x.ToString() + ", y = " + y.ToString())

    let doInjection (f:ModuleDefinition->unit) =
        let reflectedAssm = Assembly.GetExecutingAssembly()
        let assmLocation = reflectedAssm.Location
        let assembly = AssemblyDefinition.ReadAssembly(assmLocation)
        let mainModule = assembly.MainModule
        
        f mainModule

        let domainCodebase = AppDomain.CurrentDomain.BaseDirectory + "\\Patched"
        let updatedFilename = domainCodebase + "\\Tests.dll"
        if (not (Directory.Exists domainCodebase)) then
            ignore(Directory.CreateDirectory domainCodebase)
        assembly.Write(updatedFilename)
        let domain = AppDomain.CreateDomain("injected",Security.Policy.Evidence(), domainCodebase, "", false)

        new DisposableDomainWrapper(domain)

    [<Test>]
    let testInjectMethodBegin () =
        use domain = doInjection (fun mainModule ->
            let types = mainModule.Types
            let targetType = types |> Seq.filter(fun x -> x.FullName.Equals("BytecodeTools.Tests.InjectTests")) |> Seq.exactlyOne
            let targetMethod = targetType.Methods |> Seq.filter(fun x->x.Name = "injectionTarget1") |> Seq.exactlyOne

            let injectedCallTargetRef = Expr.MethodRefFromLambda ((fun () -> injectedCallTarget 2 3),mainModule)
            let updatedTarget = patchMethodBegin targetMethod injectedCallTargetRef
            ())

        let targetInstance = domain.CreateInstance<CrossDomainMethods>()

        let (result, messages) = targetInstance.RunInjectionTarget1 3 4
        Assert.AreEqual("x = 3, y = 4", messages.Head)
        Assert.AreEqual(7, result)
        ()

    [<Test>]
    let testInjectMethodReturn() =
        use domain = doInjection (fun mainModule ->
            let types = mainModule.Types
            let targetType = types |> Seq.filter(fun x -> x.FullName.Equals("BytecodeTools.Tests.InjectTests")) |> Seq.exactlyOne
            let targetMethod = targetType.Methods |> Seq.filter(fun x->x.Name = "injectionTarget1") |> Seq.exactlyOne

            let injectedCallTargetRef = Expr.MethodRefFromLambda ((fun () -> injectedCallTargetForReturn 2 3 4),mainModule)
            let updatedTarget = patchMethodReturn targetMethod injectedCallTargetRef
            ())

        let targetInstance = domain.CreateInstance<CrossDomainMethods>()

        let (result, messages) = targetInstance.RunInjectionTarget1 3 4
        Assert.AreEqual("return value = 7", messages.Head)
        Assert.AreEqual(7, result)
        ()

    [<Test>]
    let testInjectMethodReturnReplace() =
        use domain = doInjection (fun mainModule ->
            let types = mainModule.Types
            let targetType = types |> Seq.filter(fun x -> x.FullName.Equals("BytecodeTools.Tests.InjectTests")) |> Seq.exactlyOne
            let targetMethod = targetType.Methods |> Seq.filter(fun x->x.Name = "injectionTarget1") |> Seq.exactlyOne

            let injectedCallTargetRef = targetType.Methods |> Seq.filter(fun x->x.Name = "injectedCallTargetForReturnReplacement")
                                        |> Seq.exactlyOne
            let updatedTarget = patchMethodReturn targetMethod injectedCallTargetRef
            ())

        let targetInstance = domain.CreateInstance<CrossDomainMethods>()

        let (result, messages) = targetInstance.RunInjectionTarget1 3 4
        Assert.AreEqual("return value = 7", messages.Head)
        Assert.AreEqual(42, result)
        () 

    [<Test>]
    let testInjectMethodBypass() =
        use domain = doInjection (fun mainModule ->
            let types = mainModule.Types
            let targetType = types |> Seq.filter(fun x -> x.FullName.Equals("BytecodeTools.Tests.InjectTests")) |> Seq.exactlyOne
            let targetMethod = targetType.Methods |> Seq.filter(fun x->x.Name = "injectionTarget1") |> Seq.exactlyOne
            let injectedCallTargetRef = targetType.Methods |> Seq.filter(fun x->x.Name = "injectedReplacement") |> Seq.exactlyOne
            let updatedTarget = patchMethodBypass targetMethod injectedCallTargetRef
            ())
        let targetInstance = domain.CreateInstance<CrossDomainMethods>()

        let (result, messages) = targetInstance.RunInjectionTarget1 3 4
        Assert.AreEqual("Injected replacement: x = 3, y = 4", messages.Head)
        Assert.AreEqual(42, result)
        ()


        
    [<Test>]
    let testInjectInstanceMethodBegin() =
        use domain = doInjection (fun mainModule ->
            let types = mainModule.Types

            // Need the nested class define above so we can patch an instance method there.
            let targetType = (types |> Seq.filter(fun x -> x.FullName.Equals("BytecodeTools.Tests.InjectTests"))
                                |> Seq.exactlyOne).NestedTypes
                                |> Seq.filter(fun x -> x.Name.Equals("CrossDomainMethods")) |> Seq.exactlyOne

            let targetMethod = targetType.Methods |> Seq.filter(fun x->x.Name = "InjectionTarget2") |> Seq.exactlyOne

            let injectedCallTargetCallRef = Expr.MethodRefFromLambda ((fun() -> (injectedCallTargetWithThis (CrossDomainMethods()) 3 5)), mainModule)
            let updatedTarget = patchMethodBegin targetMethod injectedCallTargetCallRef
            ())

        let targetInstance = domain.CreateInstance<CrossDomainMethods>()
        let (result, messages) = targetInstance.RunInjectionTarget2 3 4
        Assert.AreEqual("x = 3, y = 4", messages.Head)
        Assert.AreEqual(12, result)

    [<Test>]
    let testInjectGenericMethodBegin() =
        use domain = doInjection (fun mainModule ->
            let types = mainModule.Types
            let targetType = types |> Seq.filter(fun x -> x.FullName.Equals("BytecodeTools.Tests.InjectTests")) |> Seq.exactlyOne

            let targetMethod = targetType.Methods |> Seq.filter(fun x -> x.Name = "genericInjectionTarget") |> Seq.exactlyOne

            // Here we're injecting a call to an int -> int -> unit
            // into a function of type 'a -> 'a -> unit, and later calling the latter as string -> string -> unit.
            // The whole thing runs and prints a message in which it reports integer values that look more like
            // pointers.  Nothing in the CLR seems to have a problem with that!
            let injectedCallTargetRef = Expr.MethodRefFromLambda ((fun () -> injectedCallTarget 2 3),mainModule)


            let updatedTarget = patchMethodBegin targetMethod injectedCallTargetRef
            ()
        )
        let targetInstance = domain.CreateInstance<CrossDomainMethods>()
        let (result, messages) = targetInstance.RunGenericInjectionTarget "a" "b"
        
        ()


