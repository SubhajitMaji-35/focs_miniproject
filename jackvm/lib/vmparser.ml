open Vmast

let rm_whitespaces (str: string) : string = String.trim str

let rec rm_comments (str: string) : string =
  match String.index_opt str '/' with
  | Some i ->
    if i+1< String.length str then
      if str.[i+1] = '/' then
        String.sub str 0 i
      else
        let rest =String.sub str (i+1) (String.length str - (i+1)) in
        let prefix = String.sub str 0 (i+1) in
        prefix ^ rm_comments rest
    else
      str
  | None -> str 

let clean_line (str: string) : string = rm_comments str |> rm_whitespaces

let rec rm_empStr (lst: string list) : string list =
  match lst with
  | [] -> []
  | h :: t ->
    if h = "" then rm_empStr t
    else h :: rm_empStr t

let get_cleanLines (program: string) : string list =
  let lines = String.split_on_char '\n' program in
  let clean_lines = List.map clean_line lines in
  rm_empStr clean_lines

let parse_segment (seg : string) : Segment.t =
  match seg with
  | "argument" -> Segment.Argument
  | "local" -> Segment.Local
  | "static" -> Segment.Static
  | "constant" -> Segment.Constant
  | "this" -> Segment.This
  | "that" -> Segment.That
  | "pointer" -> Segment.Pointer
  | "temp" -> Segment.Temp
  | _ as s -> failwith ("Error segment: " ^ s)

let parse_command (line: string) : Command.t =
  let tokens = String.split_on_char ' ' line in

  match tokens with
  | ["push"; seg; index] -> Command.Push (parse_segment seg, int_of_string index)
  | ["pop"; seg; index] -> Command.Pop (parse_segment seg, int_of_string index)
                             
  | ["add"] -> Command.ACommands Command.Add
  | ["sub"] -> Command.ACommands Command.Sub
  | ["neg"] -> Command.ACommands Command.Neg
  | ["eq"] -> Command.ACommands Command.Eq
  | ["gt"] -> Command.ACommands Command.Gt
  | ["lt"] -> Command.ACommands Command.Lt
  | ["and"] -> Command.ACommands Command.And
  | ["or"] -> Command.ACommands Command.Or
  | ["not"] -> Command.ACommands Command.Not

  | ["label"; lbl] -> Command.BCommands (Command.Label, lbl)
  | ["goto"; lbl] -> Command.BCommands (Command.Goto, lbl)
  | ["if-goto"; lbl] -> Command.BCommands (Command.Ifgo, lbl)

  | ["function"; fname; nvars] -> Command.C_Function (fname, int_of_string nvars)
  | ["call"; fname; nargs] -> Command.C_Call (fname, int_of_string nargs)
  | ["return"] -> Command.Return

  | _ -> failwith ("parse error: " ^ line)

let parse_program (program: string) : 'v  Program.t =
  program |> get_cleanLines |> List.map parse_command

