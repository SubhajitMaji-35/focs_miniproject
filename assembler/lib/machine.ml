open Ast
open SymbolTable
  
(*Module jump encodes the jump part of the C Instruction and outputs a bit list [j; j; j]*)
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

(*Module destination encodes the destination part of C Instruction and outputs a bit list [d; d; d]*)
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

(*Module computation encodes the computation part of C Instruction and outputs a bit list [a; c; c; c; c; c; c]*)
module Computation = struct

  (*Submodule for Constant computation*)
  module Const = struct
    
  let encode (c: Inst.const) : int list =
    match c with
    | Zero -> [0; 1; 0; 1; 0; 1; 0]
    | One -> [0; 1; 1; 1; 1; 1; 1]
    | MinusOne -> [0; 1; 1; 1; 0; 1; 0]

 end

  (*Submodule for Unary computation*)
  module Unary = struct

    let regenc (r: Reg.t) : int list =
      match r with
      | D -> [0; 0; 0; 1; 1]
      | A -> [0; 1; 1; 0; 0]
      | M -> [1; 1; 1; 0; 0]

    let unary (u: Inst.unary) : int list =
      match u with
      | ID -> [0; 0]
      | BNeg -> [0; 1]
      | UMinus -> [1; 1]
      | Pred -> [1; 0]
      | Succ -> [1; 1]

    let succ (r : Reg.t ) : int list =
      match r with
      | D -> [0; 0; 1; 1; 1; 1; 1]
      | A -> [0; 1; 1; 0; 1; 1; 1]
      | M -> [1; 1; 1; 0; 1; 1; 1]

    let encode (r: Reg.t) (u: Inst.unary) : int list =
      match u with
      | Succ -> succ r
      | _ -> regenc r @ unary u

  end

  (*Submodule for Binary computation*)
  module Binary = struct

    let regenc (r: Reg.t) : int list =
      match r with
       | A -> [0]
       | _ -> [1]

    let binary (b: Inst.binary) : int list =
      match b with
      | Add -> [0; 0; 0; 0; 1; 0]
      | Sub -> [0; 1; 0; 0; 1; 1]
      | SubFrom -> [0; 0; 0; 1; 1; 1]
      | BAnd -> [0; 0; 0; 0; 0; 0]
      | BOr -> [0; 1; 0; 1; 0; 1]

    let encode (r: Reg.t) (b: Inst.binary) : int list =
      (regenc r) @ (binary b) 
    
  end

  (*Final computation encoding function*)
  let encode (out : Inst.output) : int list =
    match out with
    | Inst.Constant c -> Const.encode c
    | Inst.Uapply (u, r) -> Unary.encode r u
    | Inst.Bapply (b, r) -> Binary.encode r b

end

(*Module Cinst for encoding the whole C Instruction*)
module Cinst = struct

  let encode (c: Inst.cinst) : int list = match c with
    | {destination = d; output = o; jump = j} -> [1; 1; 1;] @ (Computation.encode o) @ (Destination.encode d) @ (Jump.encode j)
  
end

(*Module Ainst for encoding the A Instruction*)
module Ainst = struct

  (*This function converts a decimal value within range( 0 - (2^15 -1)) to a 15 bit binary string*)
  let bit_encoding (n : int) : string =
    if n<0 || n>=32768 then
      failwith("Address out of range: "  ^ (string_of_int n));

    let rec to_binary acc n k =
      if k<1 then  (*Base Case*)
        acc
      else
        let bit = string_of_int (n/k) in
        to_binary (acc ^ bit) (n mod k) (k/2)
    in

    let bin_string = to_binary "" n 16384 in

    (*For checking the output of to_binary*)
    if String.length bin_string = 15 then bin_string
    else
      failwith "binary conversion error"

  (*This is the final encoding for A Instruction and here the ram address is a reference type
  mutable box pointing to an integer*)
  let encode (symbol : string) (table : SymbolTable.t) (ram_addr : int ref) : string =

    (*
       In this function,
       If the symbol is a number then it returns the binary of the number
       else it looks for the value corresponding to symbol key in the symbol table and returns the value
       else if it is not found then it adds the symbol key with the next ram address value
     *)
    let final_address =
      match find table symbol with
        | Some address -> address
        | None ->
          let new_var = !ram_addr in

          SymbolTable.add table symbol new_var;
          ram_addr := !ram_addr + 1;
          new_var

    in

    "0" ^ (bit_encoding final_address)

  let encodeN (value: int) : string =
    "0" ^ (bit_encoding value)
        
end
