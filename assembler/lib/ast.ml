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
  Destination = output expression; jump
 *)

type jinst = JGT
           | JEQ
           | JGE
           | JLT
           | JNE
           | JLE
           | JMP
type cinst = { destination : unit;
               ouput       : unit;
               jump        : jinst option
             }
type 'v inst = A of 'v
             | C of cinst

let map f = function
  | A x -> A (f x)
  | C i -> C i;;
