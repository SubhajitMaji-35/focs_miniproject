
open Utils
open Vmast

let join_lists lst = List.fold_left (@) [] lst
  
let join_lines lines = String.concat "\n" lines

module Arithmetic = struct

  let encode = function
    | Command.Add -> arithmetic "add"
    | Command.Sub -> arithmetic "sub"
    | Command.Neg -> arithmetic "neg"
    | Command.And -> arithmetic "and"
    | Command.Or -> arithmetic "or"
    | Command.Not -> arithmetic "not"
    | Command.Eq -> comp_op "JEQ"
    | Command.Lt -> comp_op "JLT"
    | Command.Gt -> comp_op "JGT"
  
end

module Push = struct

  let encode = function
    | (Segment.Constant, n) -> load_constant n @ push_d
    | (Segment.Local, i) -> load_seg_value "LCL" i @ push_d
    | (Segment.Argument, i) -> load_seg_value "ARG" i @ push_d
    | (Segment.Temp, i) -> load_temp i @ push_d
    | (Segment.Pointer, i) -> load_pointer i @ push_d
    | _ -> failwith "push not completed" 
  
end

module Pop = struct

  let encode = function
    | (Segment.Local, i) -> store_to_seg "LCL" i
    | (Segment.Argument, i) -> store_to_seg "ARG" i
    | (Segment.Temp, i) -> store_to_temp i
    | (Segment.Pointer, i) -> store_pointer i
    | _ -> failwith "pop not completed"
  
end

let encoding = function
  | Command.ACommands a -> Arithmetic.encode a
  | Command.Push (seg, n) -> Push.encode (seg, n)
  | Command.Pop (seg, n) -> Pop.encode (seg, n)
  | _ -> []

let encode (program: 'v Program.t) =
  List.map encoding program |> join_lists

