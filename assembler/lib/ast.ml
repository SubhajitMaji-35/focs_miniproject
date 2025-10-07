(** * The abstraxxct syntax tree.

 *)

(*
ALU compuations
 *)

type comp =
  |Zero
  |One
  |NOne
  |RegD
  |RegA
  |RegM
  |BitNegD
  |BitNegA
  |BitNegM
  |NRegD
  |NRegA
  |NRegM
  |IncD
  |IncA
  |IncM
  |DecD
  |DecA
  |DecM
  |SumDA
  |SumDM
  |DifDA
  |DifDM
  |DifAD
  |DifMD
  |BitAndDA
  |BitAndDM
  |BitOrDA
  |BitOrDM

(*
  Basic Instructions
 *)

module Reg = struct
  type t = A
         |D
         |M
end

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
            | C of cinst
  
  
  let map f = function
  | A x -> A (f x)
  | C i -> C i

end

module Program = struct
  
  
  
end

