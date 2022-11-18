module Gerald.ConsoleApp.Utils

open System

let toUpper (s: string) = s.ToUpper()

let toLower (s: string) = s.ToUpper()

let hexToUint8 (s: string) = Convert.ToByte(s, 16)

let catOption (opt: 'a option) =
    match opt with
    | Some x -> [x]
    | None -> []

let private filterMap (f: 'a option -> 'b option) (xs: 'a option list): 'b list =
    List.map f xs
    |> List.filter Option.isSome
    |> List.map Option.get

let flatten (lst: 'a list list): 'a list = List.fold (@) [] lst

/// Applies mapper then flattens the list.
let flatMap mapper = List.map mapper >> flatten 
