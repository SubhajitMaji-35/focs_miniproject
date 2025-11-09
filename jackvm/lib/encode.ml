
open Vmast

let join_lists lst = List.fold_left (@) [] lst
  
let join_lines lines = String.concat "\n" lines

module Arithmetic = struct

  let encode = function
    | Command.Add -> [ "@SP"; "AM=M-1"; "D=M";
                       "A=A-1"; "M=M+D"]
                     
    | Command.Sub -> ["@SP"; "AM=M-1"; "D=M";
                      "A=A-1"; "M=M-D"]

    | Command.Neg -> ["@SP"; "A=A-1"; "M=-M"]

    | _ -> failwith "unimplemented still"

end

module Push = struct

  let encode = function
    | (Segment.Constant, n) -> [
                               Printf.sprintf "@%d" n;
                               "D=A";
                               "@SP"; "A=M"; "M=D";
                               "@SP"; "M=M+1";
                             ]
    | _ -> failwith "push not implemented yet"

end

let encoding = function
  | Command.ACommands a -> Arithmetic.encode a
  | Command.Push (seg, n) -> Push.encode (seg, n)
  | _ -> []

let encode (program: 'v Program.t) =
  List.map encoding program |> join_lists |> join_lines


