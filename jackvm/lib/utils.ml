


let sp_inc = ["@SP"; "M=M+1"]
let sp_dec = ["@SP"; "M=M-1"]

let push_d = [
  "@SP";
  "A=M";
  "M=D"
] @ sp_inc

let pop_d = [
  "@SP";
  "AM=M-1";
  "D=M"
]

let load_constant n = [
  Printf.sprintf "@%d" n;
  "D=A"
]

let pop_address address = [
  "@SP";
  "AM=M-1";
  "D=M";
  Printf.sprintf "@%s" address;
  "M=D"
]

let load_seg_value seg i = [
  Printf.sprintf "@%s" seg;
  "D=M";
  Printf.sprintf "@%d" i;
  "A=D+A";
  "D=M"
]

let store_to_seg seg i = [
  Printf.sprintf "@%s" seg;
  "D=M";
  Printf.sprintf "@%d" i;
  "D=D+A";
  "@R13";
  "M=D"
]
  @ pop_d
  @ ["@R13"; "A=M"; "M=D"]

let load_temp i = [
  Printf.sprintf "@R%d" (5+i);
  "D=M"
]

let store_to_temp i =[
  Printf.sprintf "@R%d" (5+i);
  "D=A";
  "@R13";
  "M=D"
] @ pop_d
  @ ["@R13"; "A=M"; "M=D"]

let load_pointer i =
  let base = if i = 0 then "THIS" else "THAT" in
  [Printf.sprintf "@%s" base; "D=M"]

let store_pointer i =
  let base = if i =0 then "THIS" else "THAT" in
  pop_d @ [Printf.sprintf "@%s" base; "M=D"]

let label name = [Printf.sprintf "(%s)" name]
let goto name = [Printf.sprintf "@%s" name; "0;JMP"]
let ifgo name = pop_d @ [Printf.sprintf "@%s" name; "D;JNE"]

let arithmetic = function
  | "add" -> ["@SP"; "AM=M-1"; "D=M"; "A=A-1"; "M=D+M"]
  | "sub" -> ["@SP"; "AM=M-1"; "D=M"; "A=A-1"; "M=M-D"]
  | "neg" -> ["@SP"; "A=M-1"; "M=-M"]
  | "and" -> ["@SP"; "AM=M-1"; "D=M"; "A=A-1"; "M=D&M"]
  | "or" -> ["@SP"; "AM=M-1"; "D=M"; "A=A-1"; "M=D|M"]
  | "not" -> ["@SP"; "A=M-1"; "M=!M"]
  | _ as s -> failwith ("error artithmetic: " ^ s)

