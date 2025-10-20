
open Ast

module Jump = struct

  let jump (j: Inst.jinst) : int list=
    match j with
    | JGT -> [0; 0; 1]
    | JEQ -> [0; 1; 0]
    | JLT -> [1; 0; 0]
    | JGE -> [0; 1; 1]
    | JLE -> [1; 1; 0]
    | JNE -> [1; 0; 1]
    | JMP -> [1; 1; 1]

  let encode (j: Inst.jinst option) : int list =
    match j with
    | Some j -> jump j
    | None -> [0; 0; 0]
    
  end

module Destination = struct

  let encoding (r: Reg.t) : int list =
    match r with
    | Reg.A -> [1; 0; 0]
    | Reg.D -> [0; 1; 0]
    | Reg.M -> [0; 0; 1]

  let rec encode (lst: Reg.t list) : int list =
    match lst with
    | [] -> [0; 0; 0]
    | x :: xs -> List.map2 (lor) (encoding x) (encode xs)
  
end
