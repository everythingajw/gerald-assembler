module Gerald.ConsoleApp.Assembler

open Gerald.ConsoleApp.Utils
open Gerald.ConsoleApp.Parser
open Microsoft.FSharp.Collections

type CompileError = DuplicateLabel of TLabel

type CompileResult<'TOk> = Result<'TOk, CompileError> 

let computeDataSectionLabels (ds: DataSection): CompileResult<Map<TLabel, RamAddress>> =
    let rec aux (sec: DataSection) (prev: RamAddress) (acc: CompileResult<Map<TLabel, RamAddress>>) =
        match acc with
        | Ok a ->
            match sec with
            | [] -> acc
            | (Bytes (lbl, by))::t ->
              if Map.containsKey lbl a
              then aux t prev (Error (DuplicateLabel lbl))
              else aux t (prev + (uint32 (List.length by))) (Map.add lbl prev a |> Ok)
            | x -> failwith $"Unimplemented data line %A{x}"
        | Error e -> acc
    
    aux ds 0u (Ok Map.empty)
