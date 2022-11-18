module Gerald.ConsoleApp.Parser

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

type TLabel = Label of string

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

let private caseSensitive = false

let private pStringC = if caseSensitive then pstring else pstringCI

let private commentChars = ["⍝"]

let private pCommentChar = List.map pStringC commentChars |> choice

let private pCommentLine = pCommentChar >>. skipRestOfLine false 

let private ws = (pchar ' ')

let private pRegister =
    [("X", RegX); ("Y", RegY); ("A", Accumulator); ("S", StackPointer); ("P", InstructionPointer)]
    |> List.map (fun t -> pStringC (fst t) >>% (snd t))
    |> choice

let private pLabel = regex "[a-zA-Z_][a-zA-Z0-9_]*" |>> TLabel.Label

let private pLabelLine = pStringC "^" >>. pLabel .>> pStringC ":" |>> LabelLine

let private pNoArgInstruction =
     [("nop", Nop); ("tax", Tax);
      ("tay", Tay); ("txa", Txa);
      ("tya", Tya); ("inc", Inc);
      ("dec", Dec)]
     |> List.map (fun (s, c) -> (pStringC s) >>. (many ws) >>% c)
     |> choice
    
let private pOneArgRegisterInstr =
    [("adc", pRegister, Adc);
     ("sbc", pRegister, Sbc);
     ("cpx", pRegister, Cpx);
     ("cpy", pRegister, Cpy)]
    |> List.map (fun (s, p, c) -> (pStringC s) >>. (many1 ws) >>. p |>> c)
    |> choice

let private pInstruction =
    pNoArgInstruction
    <|> pOneArgRegisterInstr
    <|> ((pStringC "jmp") >>. (many1 ws) >>. pLabel |>> Jmp)
    
let private pInstructionLine = pInstruction |>> InstructionLine

let private pLine =
    let meaningful = pInstructionLine <|> pLabelLine |>> Some
    let notMeaningful = pCommentLine >>% None
    many ws >>. (meaningful <|> notMeaningful) .>> many ws

let private catOption (opt: 'a option) =
    match opt with
    | Some x -> [x]
    | None -> []

let private flatten (lst: 'a list list): 'a list = List.fold (@) [] lst

/// Applies mapper then flattens the list.
let private flatMap mapper = List.map mapper >> flatten 

let private pProgram = sepEndBy1 pLine newline |>> flatMap catOption 

let runParser = run pProgram 
