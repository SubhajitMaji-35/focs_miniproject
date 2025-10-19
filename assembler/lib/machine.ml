
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
