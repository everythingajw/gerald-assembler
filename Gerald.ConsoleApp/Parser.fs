// Author: Anthony (AJ) Webster
// Pledge: I pledge my honor that I have abided by the Stevens Honor System.

module Gerald.ConsoleApp.Parser

open Gerald.ConsoleApp.Utils
open System
open FParsec

type RomAddress = uint32

type RamAddress = uint32

type Register = X of uint8

let ZeroRegisterNumber = 29uy

let ZeroRegister = X ZeroRegisterNumber

let StackPointerNumber = 30uy

let StackPointer = X StackPointerNumber

let LinkRegisterNumber = 31uy

let LinkRegister = X LinkRegisterNumber

type TLabel = string

type Argument = Immediate of uint16
              | Reg of Register
              | Label of TLabel

type Instruction = Nop
                 | Add of (Register * Register * Argument)
                 | Sub of (Register * Register * Argument)
                 | And of (Register * Register * Argument)
                 | Orr of (Register * Register * Argument)
                 | Xor of (Register * Register * Argument)
                 | Lsl of (Register * Register * Argument)
                 | Lsr of (Register * Register * Argument)
                 | Asr of (Register * Register * Argument)
                 | Ldr of (Register * Register * uint16)
                 | Sto of (Register * Register * uint16)
                 | Jez of (Register * Argument)
                 | Jnz of (Register * Argument)
                 | Jsr of Argument
                 | Jmp of Argument

type AsmLine = LabelLine of string
             | InstructionLine of Instruction

type ProgramSection = { start: uint16; program: AsmLine list }

type DataLine = Bytes of (string * uint8 list)

type DataSection = DataLine list 

type AsmProgram = { data: DataSection option; program: ProgramSection }

let instructionSizeBytes = 4u

let maxImmediate: uint16 = uint16 (1 <<< 11) - 1us

let private caseSensitive = false

let private registerToString reg =
    match reg with
    | X n -> $"X%d{n}"

let private registerNames =
    {0..31}
    |> List.ofSeq
    |> List.map (fun x -> $"X%d{x}")
    |> Set.ofList

let private instructionStrings =
    ["nop"; "add"; "sub"; "and"; "orr"; "xor"; "lsl"; "lsr"; "asr"; "ldr"; "sto"; "jmp"; "jez"; "jnz"; "jsr"; "rts"]
    |> Set.ofList

let private reservedNames =
    let otherReserved = ["program"; "data"] |> Set.ofList
    List.fold Set.union Set.empty [registerNames; instructionStrings; otherReserved]

let private reservedNamesUpper = Set.map toUpper reservedNames

let private isReserved s = Set.contains (toUpper s) reservedNamesUpper 

let private pStringC = if caseSensitive then pstring else pstringCI
    
let private skipStringC = if caseSensitive then skipString else skipStringCI

let private commentChars =
    // Sort by descending to guarantee we get multi-char comment chars.
    // Yes, we can do this by hand, but I'd rather guarantee it with
    // a cheap one-time cost.
    ["???"; "???"; "!"] |> List.sortByDescending String.length

let private pCommentChar = List.map pStringC commentChars |> choice

let private pCommentLine = pCommentChar >>. skipRestOfLine false 

let private ws = (pchar ' ') <|> tab

let private wsLine = ws <|> newline

let oneOfMultipleStrings (pairs: (string * 'a) list) =
    List.map (fun t -> pStringC (fst t) >>% (snd t)) pairs |> choice

let private pRegister =
    let xPrefix =
        pStringC "X"
        >>. manyMinMaxSatisfy 1 2 isDigit
        |>> uint8
        >>= (fun x -> if x < 0uy || x > 31uy
                      then fail $"Invalid register X%d{x}"
                      else preturn (X x))
    let special = oneOfMultipleStrings [("XZR", ZeroRegister); ("SP", StackPointer); ("LR", LinkRegister)]

    (special <|> xPrefix)

let pImmediate = pStringC "#" >>. puint16 >>= (fun n -> if n < 0us || n > maxImmediate
                                                        then fail $"Immediate %d{n} is too large"
                                                        else preturn n)

let private pLabelName =
    let first = isAsciiLetter >>|| (=) '_'
    let rest = first >>|| isDigit
    many1Satisfy2 first rest
    >>= (fun q -> if isReserved q
                  then fail "Label cannot be a reserved name"
                  else preturn q)    

let private pLabel = pStringC "^" >>. pLabelName

let pArgument = (pRegister |>> Reg) <|> (pImmediate |>> Immediate) <|> (pLabel |>> Label)

let private pNoArgInstruction = oneOfMultipleStrings [("nop", Nop)]

let pOneArgInstr conv instrMap =
    instrMap
    |> List.map (fun (s, c) -> (pStringC s) >>. (skipMany1 ws) >>. conv |>> c)
    |> choice

let pRegisterFollowedByComma = pRegister .>> skipMany ws .>> skipChar ',' .>> skipMany ws

let pAliasInstruction =
    let pRegisterArgumentPair = tuple2 pRegisterFollowedByComma pArgument 
    pStringC "mov" >>. skipMany1 ws >>. pRegisterArgumentPair >>= (fun p -> preturn <| Add (fst p, ZeroRegister, snd p))

let p3ArgInstruction =
    let pInstr name mapper = 
        let args = tuple3 pRegisterFollowedByComma pRegisterFollowedByComma pArgument
        pStringC name >>. skipMany1 ws >>. args |>> mapper
    
    [("add", Add); ("sub", Sub); ("and", And); ("orr", Orr); ("xor", Xor); ("lsl", Lsl); ("lsr", Lsr); ("asr", Asr)]
    |> List.map (fun t -> pInstr (fst t) (snd t))
    |> choice
    
let pLoadStoreInstruction =
    [("ldr", Ldr); ("sto", Sto)]
    |> List.map (fun t -> pStringC (fst t)
                          >>. skipMany1 ws
                          >>. tuple2 pRegisterFollowedByComma (skipMany ws >>. pRegister .>> skipMany ws)
                          .>>. (opt (pStringC "offset" >>. skipMany1 ws >>. pImmediate))
                          >>= (fun ((r1, r2), off) -> preturn <| (snd t) (r1, r2, Option.defaultValue 0us off)))
    |> choice

let private pLabelLine = pLabel .>> pStringC ":" |>> LabelLine

let private pUnconditionalJump = pOneArgInstr (pLabel |>> Label) [("jmp", Jmp)]

let private pConditionalJump =
    [("jez", Jez); ("jnz", Jnz)]
    |> List.map (fun t -> pStringC (fst t)
                          >>. skipMany1 ws
                          >>. tuple2 pRegisterFollowedByComma pArgument
                          |>> (snd t))
    |> choice

let private pSubroutineJump =
    let pJsr = pStringC "jsr" >>. skipMany1 ws >>. pArgument |>> Jsr
        
    let pRts =
       pStringC "rts"
       >>. skipMany1 ws
       >>. (opt pArgument)
       >>= (function
            | Some a -> preturn a
            | None -> preturn (Reg LinkRegister))
       |>> Jmp

    pJsr <|> pRts
    
let private pInstruction =
    pNoArgInstruction
    <|> pUnconditionalJump
    <|> pConditionalJump
    <|> pSubroutineJump
    <|> pAliasInstruction
    <|> p3ArgInstruction
    <|> pLoadStoreInstruction
    
let private pInstructionLine = pInstruction |>> InstructionLine

let private pOneLine contentParser =
    spaces >>. ((contentParser |>> Some) <|> (pCommentLine >>% None)) .>> spaces .>> (optional pCommentLine) .>> spaces

let private pAsmLine = (pInstructionLine <|> pLabelLine) |> pOneLine

let private pSectionCode = (many1 pAsmLine) |>> flatMap catOption 

let private pSectionHeader header =
    skipMany wsLine >>. between (skipStringC "[") (skipStringC "]") (pStringC header) .>> skipMany wsLine

let private pBlock blockParser =
    let optionalWs = skipMany wsLine //|> optional
    let skipWs p = optionalWs >>. p .>> optionalWs .>> optional pCommentLine
    let blockStart = pStringC "{" |> skipWs
    let blockEnd = pStringC "}" |> skipWs
    between blockStart blockEnd blockParser

let private pOneByte =
    let helper = manyMinMaxSatisfy 1 2 isHex
    skipMany ws >>. helper .>> skipMany ws |>> hexToUint8

let private pByteData = sepBy1 (pchar '#' >>. puint8) (skipMany ws >>. skipChar ',' >>. skipMany ws)

let private pDataLine = (pLabel .>> pStringC ":" .>> skipMany ws) .>>. pByteData |>> Bytes |> pOneLine

let private pDataCode = (many pDataLine) |>> flatMap catOption

let private pDataSection = (pSectionHeader "data") >>. (pBlock pDataCode)

let private pProgramSection = (pSectionHeader "program") >>. (pBlock pSectionCode)

let pProgram = 
    (opt pDataSection) .>>. pProgramSection .>> (optional eof)
    |>> (fun (d, p) -> { data = d; program = { start = ~~~0us;  program = p } })

let runParser = run pProgram
