namespace BytecodeTools.SSA

module Converter =
    open Mono.Cecil

    let toSSA (m:MethodDefinition) =
        if (m.HasBody) then
            let body = m.Body;
            let instructions = body.Instructions;

            ()
        else
            ()