namespace CodeInjector

open System

module Attributes =
    [<AttributeUsage(AttributeTargets.Parameter)>]
    type ReturnValueAttribute() =
        inherit Attribute()
        
    [<AttributeUsage(AttributeTargets.Parameter)>]
    type ReplaceParameterAttribute() =
        inherit Attribute()

    ()

