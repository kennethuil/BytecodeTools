namespace BytecodeTools.Tests

open NUnit.Framework
open System
open System.Reflection
open Mono.Cecil
open CodeInjector.Injector

module InjectTests =
    let injectionTarget1 x y =
        x + y

    let injectedCallTarget (x:int) (y:int) =
        Console.Out.WriteLine("x = " + x.ToString() + ", y = " + y.ToString())

    [<Test>]
    let testInjectMethodBegin () =
        let reflectedAssm = Assembly.GetExecutingAssembly()
        let assmLocation = reflectedAssm.Location
        let assembly = AssemblyDefinition.ReadAssembly(assmLocation)
        let mainModule = assembly.MainModule
        let types = mainModule.Types
        let targetType = types |> Seq.filter(fun x -> x.FullName.Equals("BytecodeTools.Tests.InjectTests")) |> Seq.exactlyOne
        let targetMethod = targetType.Methods |> Seq.filter(fun x->x.Name = "injectionTarget1") |> Seq.exactlyOne


        let exprInjectedCallTarget = Expr.Quote (fun () -> injectedCallTarget 2 3)
        let injectedCallTargetRef = getMethodRefFromLambda mainModule exprInjectedCallTarget
        let updatedTarget = patchMethodBegin targetMethod injectedCallTargetRef

        ()

