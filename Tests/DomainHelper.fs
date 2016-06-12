namespace BytecodeTools.Tests

open System

module DomainHelper =
    type DisposableDomainWrapper(domain:AppDomain) =
        interface IDisposable with
            member this.Dispose() = AppDomain.Unload(domain)

        member this.CreateInstance<'a> () =
            let assemblyName = typeof<'a>.Assembly.FullName
            let typeName = typeof<'a>.FullName

            let objTargetInstance = domain.CreateInstanceAndUnwrap(assemblyName, typeName)
            let targetInstance = objTargetInstance :?> 'a
            targetInstance



