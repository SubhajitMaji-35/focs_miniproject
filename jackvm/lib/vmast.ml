
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

  type label = string
                
  type functionName = string

  type nVars = int
  type nArgs = int

  type index = int

  type t = Push of Segment.t * index
         | Pop of Segment.t * index
         |ACommands of arithmetic
         | BCommands of branch * label
         | C_Function of functionName * nVars
         | C_call of functionName * nArgs
         | Return
  and arithmetic = Add | Sub | Neg
                 | Eq | Gt | Lt
                 | And | Or | Not
  and branch = Label | Goto | Ifgo
               
                
end

