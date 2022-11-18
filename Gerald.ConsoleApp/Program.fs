open System
open System.Collections.Generic
open FParsec.CharParsers
open Gerald.ConsoleApp.Parser

let stringJoin (sep: string) (lst: IEnumerable<'a>) = String.Join(sep, lst)

let asmProgram = stringJoin "\n" [
    "[data]{";
    //"  !     ^FavFood: DE,AD,BE,EF";
    "       ^FavFood: DE,AD,BE,EF";
    "  !     ^FavFood: DE,AD,BE,EF";
    "\n  } ";
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
    " jmp    FreshPrince  ";
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
    "\n        }  "
]

let dumpPgm (pgm: AsmProgram): unit =
    match pgm.data with
    | None -> printfn "No data section"
    | Some d -> printfn "Data section:"; for x in d do printfn $"  %A{x}"
    
    printfn "Program section:"; for x in pgm.program do printfn $"  %A{x}"

[<EntryPoint>]
let main argv =
    let res = runParser asmProgram
    match res with
    | Success(p, _, _) -> dumpPgm p
    | x -> printfn $"%A{x}"
    0
    