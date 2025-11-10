


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

let arithmetic = function
  | "add" -> ["@SP"; "AM=M-1"; "D=M"; "A=A-1"; "M=D+M"]
  | "sub" -> ["@SP"; "AM=M-1"; "D=M"; "A=A-1"; "M=M-D"]
  | "neg" -> ["@SP"; "A=M-1"; "M=-M"]
  | "and" -> ["@SP"; "AM=M-1"; "D=M"; "A=A-1"; "M=D&M"]
  | "or" -> ["@SP"; "AM=M-1"; "D=M"; "A=A-1"; "M=D|M"]
  | "not" -> ["@SP"; "A=M-1"; "M=!M"]
  | _ as s -> failwith ("error artithmetic: " ^ s)

let l_count = ref 0

let label_gen str =
  let n = !l_count in
  l_count := !l_count + 1;
  Printf.sprintf "%s_%d" str n

let comp_op jump =
  let l_true = label_gen "TRUE" in
  let l_end = label_gen "END" in

  [
    "@SP";
    "AM=M-1";
    "D=M";
    "A=A-1";
    "D=M-D";
    Printf.sprintf "@%s" l_true;
    Printf.sprintf "D;%s" jump;
    "@SP";
    "A=M-1";
    "M=0";
    Printf.sprintf "@%s" l_end;
    "0;JMP";
    Printf.sprintf "(%s)" l_true;
    "@SP";
    "A=M-1";
    "M=-1";
    Printf.sprintf "(%s)" l_end
  ]


let funclabel_gen fname lname =
  if fname ="" then lname else fname ^ "$" ^ lname

let b_label lname fname =
  let fullname = funclabel_gen fname lname in
  [
    Printf.sprintf "(%s)" fullname
  ]

let b_goto lname fname =
  let fullname = funclabel_gen fname lname in
  [
    Printf.sprintf "@%s" fullname;
    "0;JMP"
  ]

let b_ifgo lname fname =
  let fullname = funclabel_gen fname lname in

  pop_d
  @ [
    Printf.sprintf "@%s" fullname;
    "D;JNE"
  ]


(*Function Defination*)
let rec init_locals n =
  if n<=0 then []
  else [
    "@0";
    "D=A";
    "@SP";
    "A=M";
    "M=D";
    "@SP";
    "M=M+1"
  ] @ init_locals (n-1)

let function_def fname nVars =
  [Printf.sprintf "(%s)" fname]
  @ init_locals nVars

(*Function Call*)
let function_call fname nArgs =
  let l_return = label_gen (fname ^ "$ret") in
  [
    Printf.sprintf "@%s" l_return; "D=A"; "@SP"; "A=M"; "M=D"; "@SP"; "M=M+1";
    "@LCL"; "D=M"; "@SP"; "A=M"; "M=D"; "@SP"; "M=M+1";
    "@ARG"; "D=M"; "@SP"; "A=M"; "M=D"; "@SP"; "M=M+1";
    "@THIS"; "D=M"; "@SP"; "A=M"; "M=D"; "@SP"; "M=M+1";
    "@THIS"; "D=M"; "@SP"; "A=M"; "M=D"; "@SP"; "M=M+1";

    "@SP"; "D=M"; Printf.sprintf "@%d" (nArgs + 5);
    "D=D-A"; "@ARG"; "M=D";
    "@SP"; "D=M"; "@LCL"; "M=D";

    Printf.sprintf "@%s" fname; "0;JMP";
    Printf.sprintf "(%s)" l_return;
  ]

(*Function Return*)
let function_return =
  [
    "@LCL"; "D=M"; "R13"; "M=D";                    (* FRAME = LCL*)
    "@5"; "A=D-A"; "D=M"; "@R14"; "M=D";            (* RET = *(FRAME - 5)*)
    "@SP"; "AM=M-1"; "D=M"; "@ARG"; "A=M"; "M=D";   (*  *ARG = pop()*)
    "@ARG"; "D=M+1"; "@SP"; "M=D";                  (* SP = ARG + 1*)
    "@R13"; "AM=M-1"; "D=M"; "@THAT"; "M=D";        (* THAT = *(FRAME-1)*)
    "@R13"; "AM=M-1"; "D=M"; "@THIS"; "M=D";        (* THIS = *(FRAME-2)*)
    "@R13"; "AM=M-1"; "D=M"; "@ARG"; "M=D";         (* ARG = *(FRAME-3)*)
    "@R13"; "AM=M-1"; "D=M"; "@LCL"; "M=D";         (* LCL = *(FRAME-4)*)
    "@R14"; "A=M"; "0;JMP";                         (* goto RET*)
  ]

(*Bootstrap Code*)
let bootstrap () =
  [
    "@256"; "D=A"; "@SP"; "M=D"
  ]
  @ function_call "Sys.init" 0

