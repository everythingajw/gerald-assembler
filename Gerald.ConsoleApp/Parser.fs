module Gerald.ConsoleApp.Parser

open System
open FParsec

(*
-- It's called an instruction pointer because I'm sane.
data Register = InstructionPointer | StackPointer | RegX | RegY deriving (Show)

data Label = Lab String

instance Show (Label) where
    show :: Label -> String
    show (Lab x) = x

-- http://www.6502.org/tutorials/6502opcodes.html#CPX

data Instruction = Nop 
                 | Adc Register
                 | Sbc Register
                 | Cpx Register
                 | Cpy Register
                 | Tax
                 | Tay
                 | Txa
                 | Tya
                 | Inc
                 | Dec
                 | Jmp Label
                 deriving (Show)

data AsmLine = LabelLine Label
             | InstructionLine Instruction
             deriving (Show)

data AsmProgram = Program [AsmLine] 
                  deriving (Show)
                  *)
                  
type Register = InstructionPointer | StackPointer | Accumulator | RegX | RegY

type TLabel = Label of string // override t.ToString() = (match t with | Label x -> $"%s{x}")

type Instruction = Nop 
                 | Adc of Register
                 | Sbc of Register
                 | Cpx of Register
                 | Cpy of Register
                 | Tax
                 | Tay
                 | Txa
                 | Tya
                 | Inc
                 | Dec
                 | Jmp of TLabel 

type AsmLine = LabelLine of TLabel
             | InstructionLine of Instruction

type ProgramSection = AsmLine list

type DataLine = Bytes of (TLabel * byte list)

type DataSection = DataLine list 

type AsmProgram = { data: DataSection option; program: ProgramSection }

let private caseSensitive = false

let private pStringC = if caseSensitive then pstring else pstringCI
    
let private skipStringC = if caseSensitive then skipString else skipStringCI

let private commentChars =
    ["⍝"; "!"]
    |> List.sortByDescending String.length

let private pCommentChar = List.map pStringC commentChars |> choice

let private pCommentLine = pCommentChar >>. skipRestOfLine false 

let private ws = (pchar ' ') <|> tab

let private wsLine = ws <|> newline

let private pRegister =
    [("X", RegX); ("Y", RegY); ("A", Accumulator); ("S", StackPointer); ("P", InstructionPointer)]
    |> List.map (fun t -> pStringC (fst t) >>% (snd t))
    |> choice

let private pLabelName = regex "[a-zA-Z_][a-zA-Z0-9_]*" |>> TLabel.Label

let private pLabel = pStringC "^" >>. pLabelName .>> pStringC ":"

let private pLabelLine = pLabel |>> LabelLine

let private pNoArgInstruction =
     [("nop", Nop); ("tax", Tax);
      ("tay", Tay); ("txa", Txa);
      ("tya", Tya); ("inc", Inc);
      ("dec", Dec)]
     |> List.map (fun (s, c) -> (pStringC s) >>. (many ws) >>% c)
     |> choice

let pOneArgInstr conv instrMap =
    instrMap
    |> List.map (fun (s, c) -> (pStringC s) >>. (many1 ws) >>. conv |>> c)
    |> choice

let private pOneArgRegisterInstr =
    [("adc", Adc);
     ("sbc", Sbc);
     ("cpx", Cpx);
     ("cpy", Cpy)]
    |> pOneArgInstr pRegister

let private pInstruction =
    pNoArgInstruction
    <|> pOneArgRegisterInstr
    <|> (pOneArgInstr pLabelName [("jmp", Jmp)])
    
let private pInstructionLine = pInstruction |>> InstructionLine

let private pOneLine contentParser =
    skipMany ws >>. ((contentParser |>> Some) <|> (pCommentLine >>% None)) .>> skipMany ws .>> (optional pCommentLine) .>> skipMany newline 

let private pAsmLine = pInstructionLine <|> pLabelLine |> pOneLine

let private catOption (opt: 'a option) =
    match opt with
    | Some x -> [x]
    | None -> []

let private flatten (lst: 'a list list): 'a list = List.fold (@) [] lst

/// Applies mapper then flattens the list.
let private flatMap mapper = List.map mapper >> flatten 

// let private pSectionCode = (sepEndBy1 pAsmLine newline) |>> flatMap catOption 
let private pSectionCode = (many1 pAsmLine) |>> flatMap catOption 

let private pSectionHeader header =
    skipMany wsLine >>. between (skipStringC "[") (skipStringC "]") (pStringC header) .>> skipMany wsLine

let private pBlock blockParser =
    let optionalWs = skipMany wsLine //|> optional
    let skipWs p = optionalWs >>. p .>> optionalWs .>> optional pCommentLine
    let blockStart = pStringC "{" |> skipWs
    let blockEnd = pStringC "}" |> skipWs
    between blockStart blockEnd blockParser

let toUpper (s: string) = s.ToUpper()

let toLower (s: string) = s.ToUpper()

let hexToUint8 (s: string) = Convert.ToByte(s, 16)

// let private pOneByte = skipMany ws >>. regex "[0-9a-fA-F]{1,2}" .>> skipMany ws |>> hexToUint8

let private pOneByte =
    let helper = manyMinMaxSatisfy 1 2 isHex
    skipMany ws >>. helper .>> skipMany ws |>> hexToUint8

let private pByteData = sepBy1 pOneByte (pStringC ",")

let private pDataLine = pLabel .>>. pByteData |>> Bytes |> pOneLine

// let private pDataCode = (sepEndBy pDataLine newline) |>> flatMap catOption
let private pDataCode = (many pDataLine) |>> flatMap catOption

let private pDataSection = (pSectionHeader "data") >>. (pBlock pDataCode)

let private pProgramSection = (pSectionHeader "program") >>. (pBlock pSectionCode)

let private pProgram = 
    (opt pDataSection) .>>. pProgramSection .>> (optional eof)
    |>> (fun (d, p) -> { data = d; program = p })

let runParser = run pProgram
