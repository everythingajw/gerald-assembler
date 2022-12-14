// Author: Anthony (AJ) Webster
// Pledge: I pledge my honor that I have abided by the Stevens Honor System.

module Gerald.ConsoleApp.Utils

open System

let toUpper (s: string) = s.ToUpper()

let toLower (s: string) = s.ToUpper()

let hexToUint8 (s: string) = Convert.ToByte(s, 16)

let hexToUint16 (s: string) = Convert.ToUInt16(s, 16)

let flip f a b = f b a

let (>>||) (f1: 'a -> bool) (f2: 'a -> bool) (c: 'a): bool = f1 c || f2 c  

let prepend elem lst = elem::lst

let hasAtLeastTwoElements lst =
    match lst with
    | [_, _] -> true
    | _::_::_ -> true
    | _ -> false

let catOption (opt: 'a option) =
    match opt with
    | Some x -> [x]
    | None -> []

let private filterMap (f: 'a -> 'b option) (xs: 'a list): 'b list =
    List.map f xs
    |> List.filter Option.isSome
    |> List.map Option.get

let private filterMap2 (f: 'a option -> 'b option) (xs: 'a option list): 'b list =
    let rec helper f xs acc =
        match xs with
        | [] -> List.rev acc
        | h::t ->
            match f h with
            | Some x -> helper f t (x::acc)
            | None -> helper f t acc
    helper f xs []

let flatten (lst: 'a list list): 'a list = List.fold (@) [] lst

/// Applies mapper then flattens the list.
let flatMap mapper = List.map mapper >> flatten 

let setBitU32 n = (|||) (1u <<< n)

let clearBitU32 n = (&&&) ~~~(1u <<< n)

let setBitsU32 (bits: int list) (n: uint32) =
    let folder (acc: uint32) (curr: int): uint32 = setBitU32 curr acc
    List.fold folder n bits

let getUint32BytesBigEndian (u: uint32): (uint8 * uint8 * uint8 * uint8) =
    (uint8 ((u &&& (0xFFu <<< 24)) >>> 24),
     uint8 ((u &&& (0xFFu <<< 16)) >>> 16),
     uint8 ((u &&& (0xFFu <<< 8)) >>> 8),
     uint8 ((u &&& (0xFFu <<< 0)) >>> 0))
    
let getUint32BytesLittleEndian (u: uint32) =
    let (a, b, c, d) = getUint32BytesBigEndian u
    (d, c, b, a)

let getUint32BitsBigEndian (u: uint32) = [31..-1..0] |> List.map (fun i -> (u &&& (1u <<< i)) >>> i)
    
let padRight (elem: 'a) (len: int) (lst: 'a list): 'a list =
    let listLen = List.length lst
    let toGo = len - listLen
    if toGo <= 0
    then lst
    else lst @ List.init toGo (fun _ -> elem)
