
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
    | _ -> failwith "error arithmetic encode"
  
end

module Push = struct

  let encode = function
    | (Segment.Constant, n) -> load_constant n @ push_d
    | (Segment.Local, i) -> load_seg_value "LCL" i @ push_d
    | _ -> failwith "push not completed" 
  
end

module Pop = struct

  let encode = function
    | (Segment.Local, i) -> store_to_seg "LCL" i
    | _ -> failwith "pop not completed"
  
end


let encoding = function
  | Command.ACommands a -> Arithmetic.encode a
  | Command.Push (seg, n) -> Push.encode (seg, n)
  | Command.Pop (seg, n) -> Pop.encode (seg, n)
  | _ -> []

let encode (program: 'v Program.t) =
  List.map encoding program |> join_lists




