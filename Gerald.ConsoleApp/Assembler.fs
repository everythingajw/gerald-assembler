module Gerald.ConsoleApp.Assembler

open Gerald.ConsoleApp.Utils
open Gerald.ConsoleApp.Parser
//
// let private extractJmpLabel (n: AsmLine) =
//     match n with
//     | InstructionLine (Jmp b) -> Some b
//     | _ -> None
//
// let allLabelsDefined (labels: TLabel Set) (jumpInstrs: ProgramSection): bool =
//     jumpInstrs
//     |> flatMap (extractJmpLabel >> catOption)
//     |> List.forall (fun x -> Set.contains x labels)
//     
// let getAllLabels (prog: AsmProgram): TLabel Set =
//     let dataLabels: TLabel list =
//         let labelMap (x: DataLine) = match x with Bytes b -> b
//         match prog.data with
//         | None -> []
//         | Some ds -> List.map (labelMap >> fst) ds
//
//     let progLabels: TLabel list =
//         let labelMap (n: AsmLine) =
//             match n with
//             | LabelLine t -> Some t
//             | InstructionLine (Jmp t) -> Some t
//             | _ -> None
//         flatMap (labelMap >> catOption) prog.program
//         
//     dataLabels @ progLabels |> Set.ofList
