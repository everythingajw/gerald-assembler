module Gerald.ConsoleApp.Parser

open Gerald.ConsoleApp.Utils
open System
open FParsec

type Register = X of uint8

let ZeroRegister = X 29uy

let StackPointer = X 30uy

let LinkRegister = X 31uy

type TLabel = Label of string

type Argument = Immediate of uint16
              | Reg of Register

type Instruction = Nop
                 | Add of (Register * Register * Argument)
                 | Sub of (Register * Register * Argument)
                 | And of (Register * Register * Argument)
                 | Orr of (Register * Register * Argument)
                 | Xor of (Register * Register * Argument)
                 | Jmp of TLabel
                 
// type TDirective =

type AsmLine = LabelLine of TLabel
             | InstructionLine of Instruction

type ProgramSection = { start: uint16; program: AsmLine list }

type DataLine = Bytes of (TLabel * uint8 list)

type DataSection = DataLine list 

type AsmProgram = { data: DataSection option; program: ProgramSection }

let instructionSizeBytes = 4

let maxImmediate: uint16 = uint16 (1 <<< 14) - 1us

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
    ["nop"; "add"; "sub"; "and"; "orr"; "xor"; "jmp"]
    |> Set.ofList

let private reservedNames = List.fold Set.union Set.empty [registerNames; instructionStrings]

let private reservedNamesUpper = Set.map toUpper reservedNames

let private isReserved s = Set.contains (toUpper s) reservedNamesUpper 

let private pStringC = if caseSensitive then pstring else pstringCI
    
let private skipStringC = if caseSensitive then skipString else skipStringCI

let private commentChars =
    // Sort by descending to guarantee we get multi-char comment chars.
    // Yes, we can do this by hand, but I'd rather guarantee it with
    // a cheap one-time cost.
    ["⍝"; "!"] |> List.sortByDescending String.length

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
                                                        else Immediate n |> preturn)

let pArgument = (pRegister |>> Reg) <|> pImmediate

let private pNoArgInstruction = oneOfMultipleStrings [("nop", Nop)]

let pOneArgInstr conv instrMap =
    instrMap
    |> List.map (fun (s, c) -> (pStringC s) >>. (skipMany1 ws) >>. conv |>> c)
    |> choice

let p3ArgInstruction =
    let pInstr name mapper = 
        let args =
            let pReg = pRegister .>> skipMany ws .>> pchar ',' .>> skipMany ws
            tuple3 pReg pReg pArgument
        
        pStringC name >>. skipMany1 ws >>. args |>> mapper
    
    [("add", Add); ("sub", Sub); ("and", And); ("orr", Orr); ("xor", Xor)]
    |> List.map (fun t -> pInstr (fst t) (snd t))
    |> choice

let private pLabelName =
    let first = isAsciiLetter >>|| (=) '_'
    let rest = first >>|| isDigit
    many1Satisfy2 first rest
    >>= (fun q -> if isReserved q
                  then fail "Label cannot be a reserved name"
                  else preturn (Label q))    

let private pLabel = pStringC "^" >>. pLabelName

let private pLabelLine = pLabel .>> pStringC ":" |>> LabelLine

let private pInstruction =
    pNoArgInstruction
    <|> (pOneArgInstr pLabel [("jmp", Jmp)])
    <|> p3ArgInstruction
    
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

let private pByteData = sepBy1 pOneByte (pStringC ",")

let private pDataLine = (pLabel .>> pStringC ":") .>>. pByteData |>> Bytes |> pOneLine

let private pDataCode = (sepBy pDataLine newline) |>> flatMap catOption

let private pDataSection = (pSectionHeader "data") >>. (pBlock pDataCode)

let private pProgramSection = (pSectionHeader "program") >>. (pBlock pSectionCode)

let private pProgram = 
    (opt pDataSection) .>>. pProgramSection .>> (optional eof)
    |>> (fun (d, p) -> { data = d; program = { start = ~~~0us;  program = p } })

let runParser = run pProgram
