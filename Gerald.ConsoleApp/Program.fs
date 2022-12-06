open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text
open FParsec.CharParsers
open FParsec.Error
open Gerald.ConsoleApp
open Gerald.ConsoleApp.Parser
open Gerald.ConsoleApp.Assembler
open Gerald.ConsoleApp.Utils

let stringJoin (sep: string) (lst: IEnumerable<'a>) = String.Join(sep, lst)

let asmProgram = stringJoin "\n" [
    "[data]{"
    //"  !     ^FavFood: DE,AD,BE,EF"
    "       ^FavFood: #0xDE, #0xAD, #0xBE, #0xEF"
    "       ^FavFood2: #0xDE, #0xAD, #0xBE, #0xEF"
    "       ^FavFood3: #0xDE, #0xAD, #0xBE, #0xEF"
    "  !     ^FavFood: #0xDE, #0xAD, #0xBE, #0xEF"
    "\n  } "
    "[program] {"
    "mov X0, #132"
    "add XZR, X12, X23"
    "sub XZR, X1, X2"
    "^JazzyJeff:"
    "^FreshPrince:"
    "and XZR, X1, X2"
    "orr XZR, X1, X2"
    "xor XZR, X1, X2"
    "xor XZR, X1, #0b101"  // 5
    "xor XZR, X1, #0o754"  // 492
    "xor XZR, X1, #0xA"  // 10
    "xor XZR, X1, #1618"  // 1618
    "jmp ^JazzyJeff"
    "jmp    ^FreshPrince  "
    "jez  X18,  ^FreshPrince  "
    "jnz  X19,  ^FreshPrince  "
    "ldr X10, X11"
    "ldr X10, X11 offset #0"
    "ldr X10, X11 offset #15"
    "ldr X10, X11 offset #0x15"
    "sto X13, X0"
    "sto X13, X0 offset #1"
    "sto X13, X0 offset #0x65"
    "! comment"
    "!comment1"
    "!adc X"
    "^BelAir:"
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

type AssemblerError = IOError of Exception
                    | ParseError of (string * ParserError)
                    | CompileError of Assembler.CompileError

type AssemblerResult = Result<(AsmProgram * uint32 list), AssemblerError>

let compileParsed (parserResult): AssemblerResult =
    match parserResult with
    | Success (p, _, _) ->
        computeLabelAddresses p
        |> Result.bind (processProgram p) 
        |> Result.bind assembleProgram
        |> Result.map (fun x -> (p, x))
        |> Result.mapError CompileError
    | Failure (msg, err, _) -> Error (ParseError (msg, err))

let parseAndCompileString (str: string): AssemblerResult =
    runParser str |> compileParsed
    
let parseAndCompileFile (filePath: string): AssemblerResult =
    try
        let parseRes = runParserOnFile pProgram () filePath Encoding.UTF8
        compileParsed parseRes
    with e -> Error (IOError e)

let imageHeader = "v3.0 hex words addressed"

let imagesAsHex = true

let generateRomImage (compiled: uint32 list) =
    let opcodeToString opc =
        let a, b, c, d = getUint32BytesBigEndian opc
        if imagesAsHex
        then $"%02x{a}%02x{b}%02x{c}%02x{d}"
        else $"%s{Convert.ToString(a, 2).PadRight(8, '0')}%s{Convert.ToString(b, 2).PadRight(8, '0')}%s{Convert.ToString(c, 2).PadRight(8, '0')}%s{Convert.ToString(d, 2).PadRight(8, '0')}"
    
    let zero4Bytes = String.init 8 (fun _ -> "0")
    
    compiled
    |> List.map opcodeToString
    |> padRight zero4Bytes 0x10000
    |> List.chunkBySize 8
    |> List.mapi (fun i c -> sprintf "%04x: %s" (i * 8) (stringJoin " " c))
    |> prepend imageHeader
    |> stringJoin "\n"

let generateRamImage (ds: DataSection option) =
    Option.defaultValue [] ds
    |> flatMap (function | Bytes b -> snd b)
    |> padRight 0uy (0x20000 - 16)
    |> List.map (fun b -> $"%02x{b}")
    |> List.chunkBySize 16
    |> List.mapi (fun i c -> sprintf "%04x: %s" (i * 8) (stringJoin " " c))
    |> prepend imageHeader
    |> stringJoin "\n"
    
type ExitCode =
    | AllOk = 0
    | Unknown = 1
    | InvalidArguments = 2
    | ParseError = 3
    | CompileError = 4
    | IOError = 5

let writeAllText filePath text: Result<unit, Exception> =
    try
        File.WriteAllText(filePath, text)
        Ok()
    with e -> Error e

let writeRomImage filePath image =
    Result.mapError (fun (e: Exception) -> e.Message) (writeAllText filePath image)
    
let writeRamImage filePath image =
    Result.mapError (fun (e: Exception) -> e.Message) (writeAllText filePath image)


let usageStr = $"""
Usage: %s{Environment.GetCommandLineArgs()[0]} <source> <rom> <ram>
Options:
    source    The path to the file to assemble
    rom       The path to the generated ROM image
    ram       The path to the generated RAM image
"""

[<EntryPoint>]
let main argv =
    let main' inFilePath romFilePath ramFilePath =
        printfn $"Assembly file: %s{inFilePath}"
        printfn $"ROM file: %s{romFilePath}"
        printfn $"RAM file: %s{ramFilePath}"
        
        if not (File.Exists inFilePath)
        then
            eprintfn "Error: assembly file does not exist"
            int ExitCode.IOError
        else
            let totalTimeWatch = Stopwatch.StartNew()
            let compileTimer = Stopwatch.StartNew()
            let parseCompileRes = parseAndCompileFile inFilePath
            compileTimer.Stop()
            
            match parseCompileRes with
            | Ok (parsed, compiled) ->
                printfn $"Compiled in %.03f{compileTimer.Elapsed.TotalSeconds} seconds."
                let romImageTimer = Stopwatch.StartNew()
                let romImg = generateRomImage compiled
                romImageTimer.Stop()
                printfn $"ROM image generated in %.03f{romImageTimer.Elapsed.TotalSeconds} seconds."
                match writeRomImage romFilePath romImg with
                | Ok _ ->
                    let ramImageTimer = Stopwatch.StartNew()
                    let ramImg = generateRamImage parsed.data
                    ramImageTimer.Stop()
                    printfn $"RAM image generated in %.03f{romImageTimer.Elapsed.TotalSeconds} seconds."
                    match writeAllText ramFilePath ramImg with
                    | Ok () ->
                        totalTimeWatch.Stop()
                        printfn $"Total time: %.03f{totalTimeWatch.Elapsed.TotalSeconds} seconds"
                        printfn "Assembly complete."
                        int ExitCode.AllOk
                    | Error e ->
                        eprintfn $"%s{e.Message}"
                        int ExitCode.IOError
                | Error e ->
                    eprintfn $"%s{e}"
                    int ExitCode.IOError
            | Error (IOError e) ->
                eprintfn $"I/O error: %s{e.Message}"
                int ExitCode.IOError
            | Error (ParseError (msg, _)) ->
                // eprintfn $"Syntax error: %s{msg}"
                eprintfn $"%s{msg}" // No need to print "syntax error" as the message will say that for us.
                int ExitCode.ParseError
            | Error (CompileError err) ->
                eprintfn $"Compile error: %A{err}"
                int ExitCode.ParseError
    
    if argv.Length <> 3
    then
        printfn $"%s{usageStr}"
        int ExitCode.InvalidArguments
    else
        main' argv[0] argv[1] argv[2]
        
