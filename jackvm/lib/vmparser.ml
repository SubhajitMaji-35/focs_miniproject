open Vmast

(*Helper function to remove whitespaces*)
let rm_whitespaces (str: string) : string = String.trim str

(*Helper function to remove comments*)
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

(*This function cleans the given string - removes all the comments and then whitspaces*)
let clean_line (str: string) : string = rm_comments str |> rm_whitespaces

(*This function removes all the blank lines*)
let rec rm_empStr (lst: string list) : string list =
  match lst with
  | [] -> []
  | h :: t ->
    if h = "" then rm_empStr t
    else h :: rm_empStr t

(*
   This function takes a program string
   gets the list of all the lines
   cleans all the individual lines
   removes all the blank lines
   returns the list of clean lines of the program
*)           
let get_cleanLines (program: string) : string list =
  let lines = String.split_on_char '\n' program in
  let clean_lines = List.map clean_line lines in
  rm_empStr clean_lines

(*Helper function to remove all the empty tokens - ""  *)
let rec get_cleanTokens (tokens: string list) : string list =
  match tokens with
  | [] -> []
  | h :: t -> if h = "" then get_cleanTokens t
    else
      h :: get_cleanTokens t

(*This is a helper function to return the AST of a segment type*)
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

(*
   This function takes a line tokenizes it into individual tokens by splitting the line based on ' '
   then gets clean tokens by removing all the empty tokens
   then matches the list of tokens to generate the corresponding AST
   Returns the AST
*)
let parse_command (line: string) : Command.t =
  let tokens = String.split_on_char ' ' line in
  let clean_tokens = get_cleanTokens tokens in

  match clean_tokens with
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

(*
   Main function to parse the whole program
   Takes the program string
   gets all the clean lines out of it
   then maps the parseing function on the list of lines to get a list of corresponding ASTs
*)
let parse_program (program: string) : 'v  Program.t =
  program |> get_cleanLines |> List.map parse_command

