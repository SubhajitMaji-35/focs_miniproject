%{
    open Ast
%}

%token  AT
%token  EQ
%token  SEMI
%token  LPAREN
%token  RPAREN
%token  PLUS
%token  MINUS
%token  AND
%token  OR
%token  NOT
%token  NEWLINE
%token  EOF

%token  A
%token  D
%token  M

%token ZERO
%token ONE

%token JGT
%token JEQ
%token JGE
%token JLT
%token JLE
%token JNE
%token JMP

%token <int> NUMBER
%token <string> SYMBOL

%type <string Inst.t> instruction
%type <string Inst.t> a_instruction
%type <string Inst.t> l_instruction
%type <Inst.cinst> c_instruction
%type <Reg.t> reg
%type <Reg.t list> dest
%type <Inst.jinst> jump
%type <Inst.output> comp


%start <string Ast.Program.program> prog

%%

prog:
  | program EOF { $1 }
  ;
program:
  | { [] } 
  | inst_check program {
      match $1 with
      | Some inst -> inst :: $2
      | None -> $2 
    }
  ;

inst_check:
  | instruction NEWLINE { Some $1 }
  | NEWLINE           { None }
  ;

instruction:
    |a_instruction { $1 }
    |c_instruction { Inst.C $1 }
    |l_instruction { $1 }
    ;

a_instruction:
    | AT SYMBOL { Inst.A $2 }
    | AT NUMBER { Inst.Ref $2 }
    ;

l_instruction:
    | LPAREN SYMBOL RPAREN { Inst.Label $2 }
    ;

c_instruction:
    | dest EQ comp SEMI jump { {destination = $1; output = $3; jump = Some $5} }
    | dest EQ comp { {destination = $1; output = $3; jump = None} }
    | comp SEMI jump { {destination = []; output = $1; jump = Some $3} }
    | comp { {destination = []; output = $1; jump = None} }
    ;

reg:
    | A { Reg.A }
    | D { Reg.D }
    | M { Reg.M }
    ;

dest:
    | A { [Reg.A] }
    | D { [Reg.D] }
    | M { [Reg.M] }
    | A D { [Reg.A; Reg.D] }
    | A M { [Reg.A; Reg.M] }
    | D M { [Reg.D; Reg.M] }
    | A D M { [Reg.A; Reg.D; Reg.M] }
    ;

jump:
    | JGE { Inst.JGE }
    | JGT { Inst.JGT }
    | JEQ { Inst.JEQ }
    | JNE { Inst.JNE }
    | JLE { Inst.JLE }
    | JLT { Inst.JLT }
    | JMP { Inst.JMP }
    ;

comp:
    | ZERO { Inst.Constant Inst.Zero }
    | ONE {  Inst.Constant Inst.One }
    | MINUS ONE { Inst.Constant Inst.MinusOne }
    | reg { Inst.Uapply (Inst.ID, $1) }
    | NOT reg { Inst.Uapply (Inst.BNeg, $2) }
    | MINUS reg { Inst.Uapply (Inst.UMinus, $2) }
    | reg PLUS ONE { Inst.Uapply (Inst.Succ, $1) }
    | reg MINUS ONE { Inst.Uapply (Inst.Pred, $1) }
    | D PLUS reg { Inst.Bapply (Inst.Add, $3) }
    | D MINUS reg { Inst.Bapply (Inst.Sub, $3) }
    | reg MINUS D { Inst.Bapply (Inst.SubFrom, $1) }
    | D AND reg { Inst.Bapply (Inst.BAnd, $3) }
    | D OR reg { Inst.Bapply (Inst.BOr, $3) }
    ;


