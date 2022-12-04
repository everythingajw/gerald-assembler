open System
open System.Collections.Generic
open FParsec.CharParsers
open Gerald.ConsoleApp.Parser

let stringJoin (sep: string) (lst: IEnumerable<'a>) = String.Join(sep, lst)

let asmProgram = stringJoin "\n" [
    "[data]{"
    //"  !     ^FavFood: DE,AD,BE,EF"
    "       ^FavFood: DE,AD,BE,EF"
    "  !     ^FavFood: DE,AD,BE,EF"
    "\n  } "
    "[program] {"
    "add XZR, X12, X23"
    "sub XZR, X1, X2"
    "and XZR, X1, X2"
    "orr XZR, X1, X2"
    "xor XZR, X1, X2"
    "xor XZR, X1, #0b101"  // 5
    "xor XZR, X1, #0o754"  // 492
    "xor XZR, X1, #0xA"  // 10
    "xor XZR, X1, #2718"  // 2718
    "jmp ^JazzyJeff"
    " jmp    ^FreshPrince  "
    "! comment"
    "!comment1"
    "!adc X"
    "jmp ^BelAir ⍝ going to bel air"
    "^WestPhilly:  ⍝ born and raised"
    "JMP ^WestPhilly  ⍝ going to where I was born and raised"
    "⍝^EasyPhilly:  ⍝ not born and raised"
    "⍝JMP EastPhilly  ⍝ not going to where I was not born and raised"
    "\n        }  "
]

let dumpPgm (pgm: AsmProgram): unit =
    match pgm.data with
    | None -> printfn "No data section"
    | Some d -> printfn "Data section:"; for x in d do printfn $"  %A{x}"
    
    printfn "Program section:"; for x in pgm.program.program do printfn $"  %A{x}"

[<EntryPoint>]
let main argv =
    printfn $"%s{asmProgram}"
    
    let res = runParser asmProgram
    match res with
    | Success(p, _, _) -> dumpPgm p
    | x -> printfn $"%A{x}"
    0
    