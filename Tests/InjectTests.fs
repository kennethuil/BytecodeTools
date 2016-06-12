namespace BytecodeTools.Tests

open NUnit.Framework
open System
open System.IO
open System.Reflection
open Mono.Cecil
open CodeInjector.Injector
open BytecodeTools.Tests.DomainHelper

module InjectTests =
    type ExampleTracer() =
        static let mutable messages = []

        static member WriteMessage (message:string) =
            messages <- message :: messages
        static member GetMessages() = messages

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

        let targetInstance = domain.CreateInstance<InjectionTargetClass>()

        let messages = targetInstance.RunInjectionTarget1 3 4
        Assert.AreEqual("x = 3, y = 4", messages.Head)
        ()




