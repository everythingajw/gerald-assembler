// Author: Anthony (AJ) Webster
// Pledge: I pledge my honor that I have abided by the Stevens Honor System.

module Gerald.ConsoleApp.Assembler

open Gerald.ConsoleApp.Utils
open Gerald.ConsoleApp.Parser
open Microsoft.FSharp.Collections

type CompileError = DuplicateLabel of TLabel
                  | UndefinedLabel of string
                  | InvalidJumpAddress of uint32
                  | OffsetIsNotImmediate
                  
let compileErrorToString err =
    match err with
    | DuplicateLabel d -> $"Duplicate label: '%s{d}'"
    | UndefinedLabel u -> $"Undefined label: '%s{u}'"
    | InvalidJumpAddress j -> $"Invalid jump address %d{j}. Are you trying to jump to an address in RAM?"
    | OffsetIsNotImmediate -> "The specified offset was not an immediate value."

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
                          | Lsl of (uint8 * uint8 * ProcessedArgument)
                          | Lsr of (uint8 * uint8 * ProcessedArgument)
                          | Asr of (uint8 * uint8 * ProcessedArgument)
                          | Ldr of (uint8 * uint8 * ProcessedArgument)
                          | Sto of (uint8 * uint8 * ProcessedArgument)
                          | Jmp of ProcessedArgument
                          | Jez of (uint8 * ProcessedArgument)
                          | Jnz of (uint8 * ProcessedArgument)
                          | Jsr of ProcessedArgument

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
            //| x -> failwith $"Unimplemented data line %A{x}"
        | e -> e
   
    aux ds 0u (Ok Map.empty)

let preprocessProgram (ps: AsmLine list): AsmLine list =
    let stackPointerInit =
        [InstructionLine <| Parser.Add (StackPointer, ZeroRegister, Parser.Immediate 0b1us);
         InstructionLine <| Parser.Lsl (StackPointer, StackPointer, Parser.Immediate 0x10us)]
    stackPointerInit @ ps

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
            | _::t -> aux t (prev + 1u) acc
        | e -> e

    aux ps 2u (Ok Map.empty)

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

let processProgram (prog: AsmProgram) (labelAddresses: LabelAddressMap) =
    let pgm = preprocessProgram prog.program.program
    
    let processArgument (arg: Argument): CompileResult<ProcessedArgument> =
        match arg with
        | Parser.Immediate i -> Ok <| ProcessedArgument.Immediate i
        | Parser.Reg (Parser.Register.X r) -> Ok <| ProcessedArgument.Reg r
        | Parser.Label b ->
            match Map.tryFind b labelAddresses with
            | Some x -> Ok <| ProcessedArgument.Address x
            | None -> Error <| UndefinedLabel b

    let process2Lift ((r1, a): Register * Argument): CompileResult<uint8 * ProcessedArgument> =
        match processArgument a with
        | Ok x ->
            match r1 with
            | (Register.X a) -> Ok (a, x)
        | Error e -> Error e
        
    let process3Lift ((r1, r2, a): Register * Register * Argument): CompileResult<uint8 * uint8 * ProcessedArgument> =
        match processArgument a with
        | Ok x ->
            match (r1, r2) with
            | (Register.X a, Register.X b) -> Ok (a, b, x)
        | Error e -> Error e
    
    let rec processSingleInstruction instr =
        match instr with
        | Parser.Nop -> Ok <| ProcessedInstruction.Add (ZeroRegisterNumber, ZeroRegisterNumber, Reg ZeroRegisterNumber)
        | Parser.Add args -> Result.map ProcessedInstruction.Add (process3Lift args) 
        | Parser.Sub args -> Result.map ProcessedInstruction.Sub (process3Lift args) 
        | Parser.And args -> Result.map ProcessedInstruction.And (process3Lift args) 
        | Parser.Orr args -> Result.map ProcessedInstruction.Orr (process3Lift args) 
        | Parser.Xor args -> Result.map ProcessedInstruction.Xor (process3Lift args)
        | Parser.Lsl args -> Result.map ProcessedInstruction.Lsl (process3Lift args)
        | Parser.Lsr args -> Result.map ProcessedInstruction.Lsr (process3Lift args)
        | Parser.Asr args -> Result.map ProcessedInstruction.Asr (process3Lift args)
        | Parser.Ldr (dest, addr, off) -> Result.map ProcessedInstruction.Ldr (process3Lift (dest, addr, Parser.Immediate off))
        | Parser.Sto (src, addr, off) -> Result.map ProcessedInstruction.Sto (process3Lift (src, addr, Parser.Immediate off))
        | Parser.Jmp j -> Result.map ProcessedInstruction.Jmp (processArgument j)
        | Parser.Jez j -> Result.map ProcessedInstruction.Jez (process2Lift j)
        | Parser.Jnz j -> Result.map ProcessedInstruction.Jnz (process2Lift j)
        | Parser.Jsr j -> Result.map ProcessedInstruction.Jsr (processArgument j)
    
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

module private StartBits =
    let opcode = 21
    let aluOp = 29
    let readIpFromReg = 28
    let writeIpToReg = 27
    let proceduralJump = 26
    let jump = 25
    let writeToMemory = 24
    let readFromMemory = 23
    let writeToRegister = 22
    let haveImmediate = 21
    let conditionalJumpFlag = 20
    let jumpIfZero = 19
    let destinationRegister = 16
    let readRegister1 = 11
    let readRegister2 = 5
    let immediate = 0
    
let rec encodeArgument (arg: ProcessedArgument) =
    match arg with
    | Address a ->
        // Addresses are basically immediates
        // FIXME: if the address is more than 11 bits, the instruction will be destroyed.
        match a with
        | Ram r -> setBitU32 StartBits.haveImmediate r
        | Rom r -> setBitU32 StartBits.haveImmediate r
    | Immediate i -> setBitU32 StartBits.haveImmediate (uint32 i)  // Immediates need the immediate bit set
    | Reg r -> (uint32 r) <<< StartBits.readRegister2

let encodeInstruction (instr: ProcessedInstruction): CompileResult<uint32> =
    let getAluOp (instr: ProcessedInstruction): uint32 =
        match instr with
        | Add _ -> 0b000u
        | Sub _ -> 0b001u
        | And _ -> 0b010u
        | Orr _ -> 0b011u
        | Xor _ -> 0b100u
        | Lsl _ -> 0b101u
        | Lsr _ -> 0b110u
        | Asr _ -> 0b111u
        | x -> failwith $"Invalid ALU operation %A{x}" 
    
    let encodeAluOp (dest, src, arg) =
        let op' = ((getAluOp instr) <<< 29) ||| ((uint32 dest) <<< 16) ||| ((uint32 src) <<< 11) ||| (encodeArgument arg)
        setBitsU32 [StartBits.writeToRegister] op'
    
    let computeJumpAddr addr =
        let addr' =
            match addr with
            | Reg r -> Ok (setBitsU32 [StartBits.readIpFromReg] ((uint32 r) <<< StartBits.readRegister1))
            | Immediate i -> Ok (setBitsU32 [StartBits.haveImmediate] (uint32 i))
            | Address a ->
                match a with
                | Ram r -> Error (InvalidJumpAddress r)
                | Rom r -> Ok (setBitU32 StartBits.haveImmediate (uint32 r))
        
        // We'll set the jump flag down here so we don't have to do it for every case.
        Result.map (setBitU32 25) addr'
        
    let computeConditionalJumpAddr (reg: uint8) (addr: ProcessedArgument) =
        Result.map ((|||) ((uint32 reg) <<< StartBits.readRegister1)) (computeJumpAddr addr)
        |> Result.map (setBitU32 StartBits.conditionalJumpFlag)
    
    match instr with
    | Add args
    | Sub args
    | And args
    | Orr args
    | Xor args
    | Lsl args
    | Lsr args
    | Asr args -> Ok (encodeAluOp args)
    | Ldr (destReg, addrReg, off) ->
        let opcode = setBitsU32 [StartBits.readFromMemory; StartBits.writeToRegister; StartBits.haveImmediate] 0u
        let destReg' = (uint32 destReg) <<< StartBits.destinationRegister
        let addrReg' = (uint32 addrReg) <<< StartBits.readRegister1
        match off with
        | Immediate i -> Ok (opcode ||| destReg' ||| addrReg' ||| (uint32 i))
        // This shouldn't happen since the parser only allows immediates for offsets.
        | _ -> Error OffsetIsNotImmediate
    | Sto (srcReg, addrReg, off) ->
        let opcode = setBitsU32 [StartBits.writeToMemory; StartBits.haveImmediate] 0u
        let srcReg' = (uint32 srcReg) <<< StartBits.destinationRegister
        let addrReg' = (uint32 addrReg) <<< StartBits.readRegister1
        match off with
        | Immediate i -> Ok (opcode ||| srcReg' ||| addrReg' ||| (uint32 i))
        // Again, this shouldn't happen since the parser only allows immediates for offsets.
        | _ -> Error OffsetIsNotImmediate
        
    // jmp    00000010000 00000 00000 xxxxxxxxxxx
    // jez    00000010000 11000 00000 xxxxxxxxxxx
    // jnz    00000010000 10000 00000 xxxxxxxxxxx
    | Jmp addr -> computeJumpAddr addr
    | Jez (reg, arg) -> Result.map (setBitU32 StartBits.jumpIfZero) (computeConditionalJumpAddr reg arg)
    | Jnz (reg, arg) -> Result.map (clearBitU32 StartBits.jumpIfZero) (computeConditionalJumpAddr reg arg)  // Make sure bit 19 is clear
    | Jsr arg ->
        Result.map (setBitsU32 [27; 25; 22; 21]) (computeJumpAddr arg)
        |> Result.map ((|||) ((uint32 LinkRegisterNumber) <<< StartBits.destinationRegister))

/// Main assembler.
let assembleProgram (pgm: ProcessedInstruction list): CompileResult<uint32 list> =
    let rec aux (pgm: ProcessedInstruction list) (acc: CompileResult<uint32 list>) = 
        match acc with
        | Ok a ->
            match pgm with
            | [] -> Ok a
            | h::t -> aux t (Result.map (flip prepend a) (encodeInstruction h))
        | Error e -> Error e

    // Add the infinite loop at the end, emulating the CPU halting.
    let maxAddress = uint32 (List.length pgm)
    let pgm' = pgm @ [Jmp (Address (Rom maxAddress))]
    Result.map List.rev <| aux pgm' (Ok [])
