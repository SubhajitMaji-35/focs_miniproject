
(*
   This is the file containing all the types and AST
*)
module Segment = struct

  type t = Argument
         | Local
         | Static
         | Constant
         | This
         | That
         | Pointer
         | Temp
  
end

module Command = struct

  type t = Push of Segment.t * int
         | Pop of Segment.t * int
         | ACommands of arithmetic
         | BCommands of branch * string
         | C_Function of string * int
         | C_Call of string * int
         | Return
  and arithmetic = Add | Sub | Neg
                 | Eq | Gt | Lt
                 | And | Or | Not
  and branch = Label | Goto | Ifgo
                        
end

module Program = struct

  type 'v t = Command.t list

end

