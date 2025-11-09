(** * The abstract syntax tree.

    It captures all the types for the whole Program
 *)


(*
  Registers - A, D, M
 *)

module Reg = struct
  type t = A
         |D
         |M
end


(*
   Basic Instructions
   - A Instruction
   _ C Instruction
   _ L Instruction (Labels)
*)

module Inst = struct

  type output = Constant of const
              | Uapply of unary * Reg.t
              | Bapply of binary * Reg.t
  and const = Zero | One | MinusOne
  and unary = BNeg | UMinus | Succ | Pred | ID
  and binary = Add | Sub | SubFrom | BAnd | BOr

  type jinst = JGT
           | JEQ
           | JGE
           | JLT
           | JNE
           | JLE
           | JMP

  type cinst = { destination : Reg.t list;
                 output      : output;
                 jump        : jinst option;
               }

  type 'v t = A of 'v
            | AN of int
            | C  of cinst
	    | Label of 'v
  
  
  let map f = function
    | A x -> A (f x)
    | AN r -> AN (f r)
    | C i -> C i
    | Label l -> Label l

end

(*
   Program type which will contain all the instructions in the whole program sequentially
*)

module Program = struct
  
  type 'v program = 'v Inst.t list
  
end


