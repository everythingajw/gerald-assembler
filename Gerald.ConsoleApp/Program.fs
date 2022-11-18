open System
open System.Collections.Generic
open FParsec.CharParsers
open Gerald.ConsoleApp.Parser

let stringJoin (sep: string) (lst: IEnumerable<'a>) = String.Join(sep, lst)

let asmProgram = stringJoin "\n" [
    "[program] {";
    "adc X";
    "sbc X";
    "cpx X";
    "cpy X";
    "jmp JazzyJeff"
    "adc X ";
    " sbc X";
    "  cpx X";
    " cpy X  ";
    " jmp    FreshPrince ";
    "! comment";
    "!comment1";
    "!adc X";
    "sbc X";
    "cpx X !copy to x";
    "cpy X ! copy";
    "jmp BelAir ⍝ going to bel air";
    "tax";
    "tay";
    "txa";
    "tya";
    "inc";
    "dec";
    "^WestPhilly:  ⍝ born and raised";
    "JMP WestPhilly  ⍝ going to where I was born and raised";
    "⍝^EasyPhilly:  ⍝ not born and raised";
    "⍝JMP EastPhilly  ⍝ not going to where I was not born and raised";
    "}  "
]

let dumpSect (s: Section): unit =
    match s with
    | DataSection xs -> printfn "Data section:"; for x in xs do printfn $"  %A{x}"
    | ProgramSection xs -> printfn "Program section:"; for x in xs do printfn $"  %A{x}"

[<EntryPoint>]
let main argv =
    let res = runParser asmProgram
    match res with
    | Success(ss, _, _) -> for s in ss do dumpSect s
    | x -> printfn $"%A{x}"
    0
    