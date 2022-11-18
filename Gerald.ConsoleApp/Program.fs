open System
open System.Collections.Generic
open FParsec.CharParsers
open Gerald.ConsoleApp.Parser

let stringJoin (sep: string) (lst: IEnumerable<'a>) = String.Join(sep, lst)

let asmProgram = stringJoin "\n" [
    "adc X";
    "sbc X";
    "cpx X";
    "cpy X";
    "jmp JazzyJeff"
    "adc X ";
    " sbc X";
    "  cpx X";
    " cpy X  ";
    " jmp    JazzyJeff";
]

[<EntryPoint>]
let main argv =
    let res = runParser asmProgram
    match res with
    | Success(asmLines, _, _) -> for x in asmLines do printfn "%A" x
    | x -> printfn "%A" x
    0
    