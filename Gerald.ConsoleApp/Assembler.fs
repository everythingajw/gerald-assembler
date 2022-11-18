module Gerald.ConsoleApp.Assembler

open Gerald.ConsoleApp.Utils
open Gerald.ConsoleApp.Parser
open Parser

let private extractJmpLabel (n: AsmLine) =
    match n with
    | InstructionLine (Jmp b) -> Some b
    | _ -> None

let allLabelsDefined (labels: TLabel Set) (jumpInstrs: ProgramSection): bool =
    jumpInstrs
    |> flatMap (extractJmpLabel >> catOption)
    |> List.forall (fun x -> Set.contains x labels)
    

