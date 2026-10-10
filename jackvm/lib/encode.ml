(*This file does the main encoding from ASTs to asm codes*)
(*Uses the Utils module for the corrsponding asm codes of commands*)
open Utils
open Vmast

(*Helper function to concatenate all the lists inside the parent list*)
let join_lists lst = List.fold_left (@) [] lst
  
let curr_func = ref ""

(*Module Arithmetic*)
module Arithmetic = struct

  let encode curr_func c = function
    | Command.Add -> arithmetic "add"
    | Command.Sub -> arithmetic "sub"
    | Command.Neg -> arithmetic "neg"
    | Command.And -> arithmetic "and"
    | Command.Or -> arithmetic "or"
    | Command.Not -> arithmetic "not"
    | Command.Eq -> comp_op "JEQ" curr_func c
    | Command.Lt -> comp_op "JLT" curr_func c
    | Command.Gt -> comp_op "JGT" curr_func c
  
end

(*Module Push*)
module Push = struct

  let encode file = function
    | (Segment.Constant, n) -> load_constant n @ push_d
    | (Segment.Local, i) -> load_seg_value "LCL" i @ push_d
    | (Segment.Argument, i) -> load_seg_value "ARG" i @ push_d
    | (Segment.Temp, i) -> load_temp i @ push_d
    | (Segment.Pointer, i) -> load_pointer i @ push_d
    | (Segment.This, i) -> load_seg_value "THIS" i @ push_d
    | (Segment.That, i) -> load_seg_value "THAT" i @ push_d
    | (Segment.Static, i) -> load_static file i @ push_d
  
end

(*Module Pop*)
module Pop = struct

  let encode file = function
    | (Segment.Local, i) -> store_to_seg "LCL" i
    | (Segment.Argument, i) -> store_to_seg "ARG" i
    | (Segment.Temp, i) -> store_to_temp i
    | (Segment.Pointer, i) -> store_pointer i
    | (Segment.This, i) -> store_to_seg "THIS" i
    | (Segment.That, i) -> store_to_seg "THAT" i
    | (Segment.Static, i) -> store_to_static file i
    | (Segment.Constant, _) -> failwith "pop constant is not a valid command"
  
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
   It takes the file name (used for static variables) and a program of AST list
   For every AST , it encodes to corresponding list of asm instructions
   While defining a function, all the labels are function scoped and hence named as <label>$<fname>
   In calling a function and in comparisons, a counter is also used , to generate unique labels in the current function
   Code outside any function is scoped by the file name, so labels of different files never clash

   Then all the lists are joined inside the parent list and then it is returned
*)
let encode (file: string) (program: 'v Program.t) =
  let rec encoding program func c =
    match program with
    | [] -> []
    | curr :: rest ->
      match curr with
      | Command.C_Function (fname, n) -> function_def fname n :: encoding rest fname c
      | Command.C_Call (fname, n) -> function_call func fname n c :: encoding rest func (c+1)
      | Command.Return -> function_return :: encoding rest func c
      | Command.ACommands a -> Arithmetic.encode func c a :: encoding rest func (c+1)
      | Command.Push (seg, n) -> Push.encode file (seg, n) :: encoding rest func c
      | Command.Pop (seg, n) -> Pop.encode file (seg, n) :: encoding rest func c
      | Command.BCommands (branch, lname) -> Branch.encode (branch, lname, func) :: encoding rest func c
  in
  encoding program file 0 |> join_lists


