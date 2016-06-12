namespace BytecodeTools.Tests

open NUnit.Framework
open System
open System.IO
open System.Reflection
open Mono.Cecil
open CodeInjector.Injector

module InjectTests =
    type ExampleTracer() =
        static let mutable messages = []

        static member WriteMessage (message:string) =
            messages <- message :: messages

        static member AssertMessage (message:string) =
            match messages with
            | [] -> failwith ("No message, expected " + message)
            | x::preceding ->
                Assert.AreEqual(message, x)
                preceding

        static member GetMessages() = messages

    let domainCodebase = AppDomain.CurrentDomain.BaseDirectory + "\\Patched"

    let injectionTarget1 x y =
        x + y

    let injectedCallTarget (x:int) (y:int) =
        ExampleTracer.WriteMessage("x = " + x.ToString() + ", y = " + y.ToString())

    // Need a class inheriting MarshalByRefObject to do cross-domain stuff
    type InjectionTargetClass() = 
        inherit MarshalByRefObject()

        do
            let domain = AppDomain.CurrentDomain
            ()

        member this.RunInjectionTarget1 x y =

            let z = injectionTarget1 x y
            ExampleTracer.GetMessages()

    let doInjection (f:ModuleDefinition->unit) =
        let reflectedAssm = Assembly.GetExecutingAssembly()
        let assmLocation = reflectedAssm.Location
        let assembly = AssemblyDefinition.ReadAssembly(assmLocation)
        //assembly.Name <- new AssemblyNameDefinition("ModifiedTestAssembly", new Version("1.0.0.0"))
        let mainModule = assembly.MainModule
        
        f mainModule

        let updatedFilename = domainCodebase + "\\Tests.dll"
        if (not (Directory.Exists domainCodebase)) then
            ignore(Directory.CreateDirectory domainCodebase)
        assembly.Write(updatedFilename)
        updatedFilename

    [<Test>]
    let testInjectMethodBegin () =
        let injectedAssm = doInjection (fun mainModule ->
            let types = mainModule.Types
            let targetType = types |> Seq.filter(fun x -> x.FullName.Equals("BytecodeTools.Tests.InjectTests")) |> Seq.exactlyOne
            let targetMethod = targetType.Methods |> Seq.filter(fun x->x.Name = "injectionTarget1") |> Seq.exactlyOne

            let injectedCallTargetRef = Expr.MethodRefFromLambda ((fun () -> injectedCallTarget 2 3),mainModule)
            let updatedTarget = patchMethodBegin targetMethod injectedCallTargetRef
            ())

        let domain = AppDomain.CreateDomain("injected",Security.Policy.Evidence(), domainCodebase, "", false)
        let objTargetInstance = domain.CreateInstanceAndUnwrap("Tests", "BytecodeTools.Tests.InjectTests+InjectionTargetClass")
        let targetInstance = objTargetInstance :?> InjectionTargetClass

        //let targetMethod = objTargetInstance.GetType().GetMethod("RunInjectionTarget1")
        //let messages = targetMethod.Invoke(objTargetInstance, [|5;6|]) :?> string list
        let messages = targetInstance.RunInjectionTarget1 3 4
        Assert.AreEqual("x = 3, y = 4", messages.Head)
        ()




