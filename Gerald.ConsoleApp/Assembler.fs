module Gerald.ConsoleApp.Assembler

open System
open Gerald.ConsoleApp.Utils
open Gerald.ConsoleApp.Parser
open Microsoft.FSharp.Collections

type CompileError = DuplicateLabel of TLabel
                  | UndefinedLabel of string
                  | InvalidJumpAddress of uint32

type CompileResult<'TOk> = Result<'TOk, CompileError> 

type LabelAddress = Ram of RamAddress
                  | Rom of RomAddress

type LabelAddressMap = Map<TLabel, LabelAddress>

type ProcessedArgument = Immediate of uint16
                       | Reg of uint8
                       | Address of LabelAddress

type ProcessedInstruction = Add of (uint8 * uint8 * ProcessedArgument)
                          | Sub of (uint8 * uint8 * ProcessedArgument)
                          | And of (uint8 * uint8 * ProcessedArgument)
                          | Orr of (uint8 * uint8 * ProcessedArgument)
                          | Xor of (uint8 * uint8 * ProcessedArgument)
                          | Jmp of ProcessedArgument

let computeDataSectionLabels (ds: DataSection): CompileResult<LabelAddressMap> =
    let rec aux (sec: DataSection) (prev: RamAddress) (acc: CompileResult<LabelAddressMap>) =
        match acc with
        | Ok labelMap ->
            match sec with
            | [] -> acc
            | (Bytes (lbl, by))::t ->
              if Map.containsKey lbl labelMap
              then aux t prev (Error (DuplicateLabel lbl))
              else aux t (prev + (uint32 (List.length by))) (Map.add lbl (Ram prev) labelMap |> Ok)
            | x -> failwith $"Unimplemented data line %A{x}"
        | e -> e
   
    aux ds 0u (Ok Map.empty)

let computeProgramSectionLabels (ps: AsmLine list): CompileResult<LabelAddressMap> =
    let rec aux (ps: AsmLine list) (prev: RomAddress) (acc: CompileResult<LabelAddressMap>) =
        match acc with
        | Ok labelMap ->
            match ps with
            | [] -> acc
            | (LabelLine lbl)::t ->
                if Map.containsKey lbl labelMap
                then aux t prev (Error <| DuplicateLabel lbl)
                else aux t prev (Ok <| Map.add lbl (Rom prev) labelMap)
            | _::t -> aux t (prev + instructionSizeBytes) acc
        | e -> e

    
    aux ps 0u (Ok Map.empty)

let computeLabelAddresses (pgm: AsmProgram): CompileResult<LabelAddressMap> = 
    let dataSectionMap =
        match pgm.data with
        | Some d -> computeDataSectionLabels d
        | None -> Ok Map.empty
    let pgmSectionMap = computeProgramSectionLabels pgm.program.program
    
    match (dataSectionMap, pgmSectionMap) with
    | (Ok d, Ok p) ->
        let joined = (Map.toList d) @ (Map.toList p)
        let moreThanOne =
            joined
            |> List.groupBy fst
            |> List.where (snd >> List.length >> (>) 1)
        
        match moreThanOne with
        | [] -> Ok <| Map.ofList joined
        | (b, _)::_ -> Error <| DuplicateLabel b
                    
    | (Error e, _)
    | (_, Error e) -> Error e
    | (Error d, Error p) -> Error d  // We shouldn't get here, but we'll add it to be safe.

let processProgram (prog: AsmProgram) (labelAddresses: LabelAddressMap) =
    let pgm = prog.program.program
    
    let processArgument (arg: Argument): CompileResult<ProcessedArgument> =
        match arg with
        | Parser.Immediate i -> Ok <| ProcessedArgument.Immediate i
        | Parser.Reg (Parser.Register.X r) -> Ok <| ProcessedArgument.Reg r
        | Parser.Label b ->
            match Map.tryFind b labelAddresses with
            | Some x -> Ok <| ProcessedArgument.Address x
            | None -> Error <| UndefinedLabel b
    
    let process3Lift (triple: Register * Register * Argument): CompileResult<uint8 * uint8 * ProcessedArgument> =
        let (r1, r2, a) = triple
        match processArgument a with
        | Ok x ->
            match (r1, r2) with
            | (Register.X a, Register.X b) -> Ok (a, b, x)
        | Error e -> Error e
    
    let processSingleInstruction instr =
        match instr with
        | Parser.Nop -> Ok <| ProcessedInstruction.Add (ZeroRegisterNumber, ZeroRegisterNumber, Reg ZeroRegisterNumber)
        | Parser.Add args -> Result.map ProcessedInstruction.Add (process3Lift args) 
        | Parser.Sub args -> Result.map ProcessedInstruction.Sub (process3Lift args) 
        | Parser.And args -> Result.map ProcessedInstruction.And (process3Lift args) 
        | Parser.Orr args -> Result.map ProcessedInstruction.Orr (process3Lift args) 
        | Parser.Xor args -> Result.map ProcessedInstruction.Xor (process3Lift args)
        | Parser.Jmp j -> Result.map ProcessedInstruction.Jmp (processArgument j)
    
    let rec aux (lines: AsmLine list) (acc: CompileResult<ProcessedInstruction list>) =
        match acc with
        | Ok a ->
            match lines with
            | [] -> Ok a
            | (LabelLine _)::t -> aux t (Ok a)
            | (InstructionLine i)::t ->
                aux t (Result.map (flip prepend a) (processSingleInstruction i))
        | Error e -> Error e

    Result.map List.rev <| aux pgm (Ok [])
