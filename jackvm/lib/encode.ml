(*This file does the main encoding from ASTs to asm codes*)
(*Uses the Utils module for the corrsponding asm codes of commands*)
open Utils
open Vmast

(*Helper function to concatenate all the lists inside the parent list*)
let join_lists lst = List.fold_left (@) [] lst
  
let curr_func = ref ""

(*Module Arithmetic*)
module Arithmetic = struct

  let encode curr_func = function
    | Command.Add -> arithmetic "add"
    | Command.Sub -> arithmetic "sub"
    | Command.Neg -> arithmetic "neg"
    | Command.And -> arithmetic "and"
    | Command.Or -> arithmetic "or"
    | Command.Not -> arithmetic "not"
    | Command.Eq -> comp_op "JEQ" curr_func
    | Command.Lt -> comp_op "JLT" curr_func
    | Command.Gt -> comp_op "JGT" curr_func
  
end

(*Module Push*)
module Push = struct

  let encode = function
    | (Segment.Constant, n) -> load_constant n @ push_d
    | (Segment.Local, i) -> load_seg_value "LCL" i @ push_d
    | (Segment.Argument, i) -> load_seg_value "ARG" i @ push_d
    | (Segment.Temp, i) -> load_temp i @ push_d
    | (Segment.Pointer, i) -> load_pointer i @ push_d
    | (Segment.This, i) -> load_seg_value "THIS" i @ push_d
    | (Segment.That, i) -> load_seg_value "THAT" i @ push_d
    | _ -> failwith "push static not implemented" 
  
end

(*Module Pop*)
module Pop = struct

  let encode = function
    | (Segment.Local, i) -> store_to_seg "LCL" i
    | (Segment.Argument, i) -> store_to_seg "ARG" i
    | (Segment.Temp, i) -> store_to_temp i
    | (Segment.Pointer, i) -> store_pointer i
    | (Segment.This, i) -> store_to_seg "THIS" i
    | (Segment.That, i) -> store_to_seg "THAT" i
    | _ -> failwith "pop static not implemented"
  
end

(*Module Branch*)
module Branch = struct

  let encode = function
    | (Command.Label, lname, curr_func) -> b_label lname curr_func
    | (Command.Goto, lname, curr_func) -> b_goto lname curr_func
    | (Command.Ifgo, lname, curr_func) -> b_ifgo lname curr_func
  
end

(*
   Main encode function
   It takes a program of AST list
   For every AST , it encodes to corresponding list of asm instructions
   While defining a function, all the labels are function scoped and hence named as <label>$<fname>
   In calling a function, a counter is also used , to generate unique lables of function return address

   Then all the lists are joined inside the parent list and then it is returned
*)
let encode (program: 'v Program.t) =
  let rec encoding program func c =
    match program with
    | [] -> []
    | curr :: rest ->
      match curr with
      | Command.C_Function (fname, n) -> function_def fname n :: encoding rest fname c
      | Command.C_Call (fname, n) -> function_call fname n c :: encoding rest func (c+1)
      | Command.Return -> function_return :: encoding rest func c
      | Command.ACommands a -> Arithmetic.encode func a :: encoding rest func c
      | Command.Push (seg, n) -> Push.encode (seg, n) :: encoding rest func c
      | Command.Pop (seg, n) -> Pop.encode (seg, n) :: encoding rest func c
      | Command.BCommands (branch, lname) -> Branch.encode (branch, lname, func) :: encoding rest func c
  in
  encoding program "" 0 |> join_lists

