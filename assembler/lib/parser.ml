open Ast

(*Defining the types - digits, symbols,etc.*)
let digits = "1234567890"

let s_alpha = "abcdefghijklmnopqrstuvwxyz"
let c_alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let special = "_.$:"
let var_start = s_alpha ^ c_alpha ^ special
let var_after = var_start ^ digits


(*Functions to check wheter the input is number or symbol*)
let rec is_number (inp: string) : bool =
  match String.length inp with
  | 0 -> failwith("invalid digit check")
  | 1 -> if String.contains digits inp.[0] then true
    else false
  | _ as l -> let rest = is_number (String.sub inp 1 (l-1)) in
                  (String.contains digits inp.[0]) && rest

let rec is_after (inp: string) : bool =
  match String.length inp with
  | 0 -> true
  | 1 -> if String.contains var_after inp.[0] then true
    else false
  | _ as l -> let rest = is_after (String.sub inp 1 (l-1)) in
    (String.contains var_after inp.[0]) && rest

let is_symbol (inp: string) : bool =
  match String.length inp with
  | 0 -> failwith "invalid symbol checker"
  | 1 -> if String.contains var_start inp.[0] then true
    else false
  | _ as l -> let rest = is_after (String.sub inp 1 (l-1)) in
    (String.contains var_start inp.[0]) && rest


(*Functions to clean the give input line*)
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


(*This function Removes the additional lines which are empty*)
let rec rm_empStr (lst: string list) : string list =
  match lst with
  | [] -> []
  | h :: t ->
    if h = "" then rm_empStr t
    else h :: rm_empStr t

(*Function to get clean lines from a given string of complete program*)
let get_cleanLines (program: string) : string list =
  let lines = String.split_on_char '\n' program in
  let clean_lines = List.map clean_line lines in
  rm_empStr clean_lines


(*
   Main parsing logic for C Instructions
   General C Instruction -> dest=comp;jump
   If jump is not present , ';' is not used
   If dest is not present , '=' is not used
   in valid every case, comp is always there

*)
let parse_cinst (str : string) : Inst.cinst =

  (*First the split will be on the basis of '=', if '=' is not present then dest is empty*)
  let parts_equal = String.split_on_char '=' str in
  let (p_dest, rest) =
    match parts_equal with
    | h :: [] -> ("",  h)
    | h :: t :: [] -> ( h, t)
    | _ -> failwith "equal error: more than 1 '=' is present"
  in

  (*Next the split will be on the basis of ';', if ';' is not present then jump is empty*)
  let parts_semi = String.split_on_char ';' rest in
  let (p_comp, p_jump) =
    match parts_semi with
    | [h] -> (h, "")
    | h :: t :: [] -> (h,t)
    | _ -> failwith "semi error: more than 1 ';' is present"
  in

  (*Matching the dest string with the types defined in ast*)
  let dest : Reg.t list =
    match p_dest with
    | "" -> [] | "A" -> [Reg.A] | "D" -> [Reg.D] | "M" -> [Reg.M]
    | "AD" -> [Reg.A; Reg.D] | "AM" -> [Reg.A; Reg.M] | "DM" | "MD" -> [Reg.D; Reg.M]
    | "ADM" | "AMD" -> [Reg.A; Reg.D; Reg.M] | _ -> failwith "invalid dest, check for additional spaces"
  in

  (*Matching the comp string with the types defined in ast*)
  let comp : Inst.output =
    match p_comp with
    | "0" -> Constant Zero | "1" -> Constant One | "-1" -> Constant MinusOne
    | "A" -> Uapply (ID, A) | "D" -> Uapply (ID, D) | "M" -> Uapply (ID, M)
    | "!A" -> Uapply (BNeg, A) | "!D" -> Uapply (BNeg, D) | "!M" -> Uapply(BNeg, M)
    | "-A" -> Uapply (UMinus, A) | "-D" -> Uapply (UMinus, D) | "-M" -> Uapply (UMinus, M)
    | "A+1" -> Uapply (Succ, A) | "D+1" -> Uapply (Succ, D) | "M+1" -> Uapply (Succ, M)
    | "A-1" -> Uapply(Pred, A) | "D-1" -> Uapply(Pred, D) | "M-1" -> Uapply(Pred, M)
    | "D+A" -> Bapply (Add, A) | "D+M" -> Bapply (Add, M)
    | "D-A" -> Bapply (Sub, A) | "D-M" -> Bapply (Sub, M)
    | "A-D" -> Bapply(SubFrom, A) | "M-D" -> Bapply (SubFrom, M)
    | "D&A" -> Bapply (BAnd, A) | "D&M" -> Bapply (BAnd, M)
    | "D|A" -> Bapply (BOr, A) | "D|M" -> Bapply (BOr, M)
    | _ as l-> failwith ("error comp: " ^ l) 
  in

  (*Matching the jump string with the types defined in ast*)
  let jump : Inst.jinst option =
    match p_jump with
    | "JGT" -> Some JGT | "JGE" -> Some JGE
    | "JEQ" -> Some JEQ | "JNE" -> Some JNE
    | "JLE" -> Some JLE | "JLT" -> Some JLT
    | "JMP" -> Some JMP | "" -> None
    | _ -> failwith "error jump, check for additional spaces"
  in

  { destination = dest; output = comp; jump = jump}


(*Helper functions to check for checking valid Instruction types*)
let check_ainst (inp: string) : bool =
  if inp.[0] = '@' && String.length inp > 1 then true
  else false

let check_linst (inp: string) : bool =
  let l= String.length inp in
  if l > 2 then
    if inp.[0] = '(' && inp.[l-1] = ')' then true
    else false
  else false

(*This functions parses input line and converts it into AST*)
let parse_line (line : string) : 'v Inst.t =
  let l = String.length line in
  if line = "" then failwith "error line parse: empty line"

  else if check_ainst line then
    let value = String.sub line 1 (l-1) in
    if is_number value then AN (int_of_string value)
    else
      A value

  else if check_linst line then
    let symbol = String.sub line 1 (l-2) in
    Label symbol

  else
    C (parse_cinst line)
(*This is the main function, which will parse the whole program string into Program of AST list*)
let parse_program (program: string) : 'v Ast.Program.program =
  let clean_lines = get_cleanLines program in
  List.map parse_line clean_lines

