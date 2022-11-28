module Gerald.ConsoleApp.Parser

open Gerald.ConsoleApp.Utils
open System
open FParsec

type Register = InstructionPointer | StackPointer | Accumulator | RegX | RegY

type TLabel = Label of string // override t.ToString() = (match t with | Label x -> $"%s{x}")

type Argument = Absolute of uint16
              | Immediate of uint8
              | Reg of Register
              | ZeroPage of uint8
              | ZeroPageOffsetX of uint8
              | ZeroPageOffsetY of uint8

type Instruction = Nop 
                 | Adc of Argument
                 | Sbc of Argument
                 | Cmp of Argument
                 | Cpx of Argument
                 | Cpy of Argument
                 | Pha
                 | Pla
                 | Tax
                 | Tay
                 | Txa
                 | Tya
                 | Sta of Argument
                 | Stx of Argument
                 | Sty of Argument
                 | Inc
                 | Dec
                 | Jmp of TLabel
                 | Jsr of TLabel
                 | Rts

type AsmLine = LabelLine of TLabel
             | InstructionLine of Instruction

type ProgramSection = AsmLine list

type DataLine = Bytes of (TLabel * byte list)

type DataSection = DataLine list 

type AsmProgram = { data: DataSection option; program: ProgramSection }

let private caseSensitive = false

let private registerToString reg =
    match reg with
    | InstructionPointer -> "IP" 
    | StackPointer -> "SP"
    | Accumulator -> "A"
    | RegX -> "X"
    | RegY -> "Y"

let private registerNames =
    [InstructionPointer; StackPointer; Accumulator; RegX; RegY]
    |> List.map registerToString
    |> Set.ofList

let private instructionStrings =
    ["nop"; "adc"; "sbc"; "cmp"; "cpx"; "cpy"; "pha"; "pla"; "tax"; "tay"; "txa"; "tya";
     "sta"; "stx"; "sty"; "inc"; "dec"; "jmp"; "jsr"; "rts";]
    |> Set.ofList

let private reservedNames = List.fold Set.union Set.empty [registerNames; instructionStrings]

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

let private pRegisterX = registerToString RegX |> pStringC >>% RegX

let private pRegisterY = registerToString RegY |> pStringC >>% RegY

let private pRegister =
    let rtsPair r = (registerToString r, r)
    let others =
        [rtsPair Accumulator; rtsPair StackPointer; rtsPair InstructionPointer]
        |> List.map (fun t -> pStringC (fst t) >>% (snd t))
        |> choice
    
    pRegisterX <|> pRegisterY <|> others    
    
let private pNumber prefix maxDigits conv = pStringC prefix >>. manyMinMaxSatisfy 1 maxDigits isHex |>> conv 

let private pUint8 = pNumber "0x" 2 hexToUint8

let private pUint16 = pNumber "0x" 4 hexToUint16

// Strict hex support
// let private pArgument =
//     // Order is important because of maximal munch
//     (pRegister |>> Argument.Reg)
//     <|> (pStringC "#" >>. pUint8 |>> Immediate)
//     <|> (pUint16 |>> Absolute)
//     <|> (pUint8 .>> pRegisterX |>> ZeroPageOffsetX)
//     <|> (pUint8 .>> pRegisterY |>> ZeroPageOffsetY)
//     <|> (pUint8 |>> ZeroPage)

let private pArgument =
    // Order is important because of maximal munch
    (pRegister |>> Argument.Reg)
    <|> (pStringC "#" >>. puint8 |>> Immediate)
    <|> (puint16 |>> Absolute)
    <|> (puint8 .>> pRegisterX |>> ZeroPageOffsetX)
    <|> (puint8 .>> pRegisterY |>> ZeroPageOffsetY)
    <|> (puint8 |>> ZeroPage)

// let private pLabelName = regex "[a-zA-Z_][a-zA-Z0-9_]*" |>> TLabel.Label

let (>>||) (f1: 'a -> bool) (f2: 'a -> bool) (c: 'a): bool = f1 c || f2 c  

let private pLabelName =
    let first = isAsciiLetter >>|| (=) '_'  //  isAsciiLetter c || c = '_' 
    let rest = first >>|| isDigit  // first c || isDigit c
    many1Satisfy2 first rest
    >>= (fun q -> if Set.contains (toLower q) reservedNames
                  then fail "Label cannot be a reserved name"
                  else preturn (Label q))

let private pLabel = pStringC "^" >>. pLabelName .>> pStringC ":"

let private pLabelLine = pLabel |>> LabelLine

let private pNoArgInstruction =
     [("nop", Nop); ("tax", Tax);
      ("tay", Tay); ("txa", Txa);
      ("tya", Tya); ("inc", Inc);
      ("dec", Dec); ("pha", Pha);
      ("pla", Pla); ("rts", Rts)]
     |> List.map (fun (s, c) -> (pStringC s) >>. (many ws) >>% c)
     |> choice

let pOneArgInstr conv instrMap =
    instrMap
    |> List.map (fun (s, c) -> (pStringC s) >>. (many1 ws) >>. conv |>> c)
    |> choice

let private pOneArgRegisterInstr =
    [("adc", Adc); ("sbc", Sbc);
     ("cpx", Cpx); ("cpy", Cpy);
     ("sta", Sta); ("stx", Stx);
     ("sty", Sty); ("cmp", Cmp)]
    |> pOneArgInstr pArgument

let private pInstruction =
    pNoArgInstruction
    <|> pOneArgRegisterInstr
    <|> (pOneArgInstr pLabelName [("jmp", Jmp); ("jsr", Jsr)])
    
let private pInstructionLine = pInstruction |>> InstructionLine

let private pOneLine contentParser =
    spaces >>. ((contentParser |>> Some) <|> (pCommentLine >>% None)) .>> spaces .>> (optional pCommentLine) .>> spaces

let private pAsmLine = pInstructionLine <|> pLabelLine |> pOneLine

let private pSectionCode = (many1 pAsmLine) |>> flatMap catOption 

let private pSectionHeader header =
    skipMany wsLine >>. between (skipStringC "[") (skipStringC "]") (pStringC header) .>> skipMany wsLine

let private pBlock blockParser =
    let optionalWs = skipMany wsLine //|> optional
    let skipWs p = optionalWs >>. p .>> optionalWs .>> optional pCommentLine
    let blockStart = pStringC "{" |> skipWs
    let blockEnd = pStringC "}" |> skipWs
    between blockStart blockEnd blockParser

// let private pOneByte = skipMany ws >>. regex "[0-9a-fA-F]{1,2}" .>> skipMany ws |>> hexToUint8

let private pOneByte =
    let helper = manyMinMaxSatisfy 1 2 isHex
    skipMany ws >>. helper .>> skipMany ws |>> hexToUint8

let private pByteData = sepBy1 pOneByte (pStringC ",")

let private pDataLine = pLabel .>>. pByteData |>> Bytes |> pOneLine

// let private pDataCode = (sepEndBy pDataLine newline) |>> flatMap catOption
let private pDataCode = (sepBy pDataLine newline) |>> flatMap catOption

let private pDataSection = (pSectionHeader "data") >>. (pBlock pDataCode)

let private pProgramSection = (pSectionHeader "program") >>. (pBlock pSectionCode)

let private pProgram = 
    (opt pDataSection) .>>. pProgramSection .>> (optional eof)
    |>> (fun (d, p) -> { data = d; program = p })

let runParser = run pProgram
