
open Ast
open SymbolTable
  

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

module Computation = struct

  module Const = struct
    
  let constant (c: Inst.const) : int list =
    match c with
    | Zero -> [0; 1; 0; 1; 0; 1; 0]
    | One -> [0; 1; 1; 1; 1; 1; 1]
    | MinusOne -> [0; 1; 1; 1; 0; 1; 0]

 end

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

  let encode (out : Inst.output) : int list =
    match out with
    | Inst.Constant c -> Const.constant c
    | Inst.Uapply (u, r) -> Unary.encode r u
    | Inst.Bapply (b, r) -> Binary.encode r b

end

module Cinst = struct

  let encode (c: Inst.cinst) : int list = match c with
    | {destination = d; output = o; jump = j} -> [1; 1; 1;] @ (Computation.encode o) @ (Destination.encode d) @ (Jump.encode j)
  
end

module Ainst = struct

  let int_to_15bitbinary (n : int) : string =
    if n<0 || n>=32768 then
      failwith("Address out of range: "  ^ (string_of_int n));

    let rec to_binary acc n k =
      if k<1 then  (*Base Case*)
        if acc ="" then "0" else acc
      else
        let bit = string_of_int (n/k) in
        to_binary (acc ^ bit) (n mod k) (k/2)
    in

    let bin_string = to_binary "" n 16384 in

    let pad_len = 15 - (String.length bin_string) in
    if pad_len < 0 then
      failwith( "Binary converion error for: " ^ (string_of_int n));

    (String.make pad_len '0') ^ bin_string

  let encode (symbol : string) (table : SymbolTable.t) (ram_addr : int ref) : string =

    let final_address =
      match int_of_string_opt symbol with
      | Some value -> value
      | None ->
        match find table symbol with
        | Some address -> address
        | None ->
          let new_var = !ram_addr in

          SymbolTable.add table symbol new_var;
          ram_addr := !ram_addr + 1;
          new_var

    in

    "0" ^ (int_to_15bitbinary final_address)
        
end
