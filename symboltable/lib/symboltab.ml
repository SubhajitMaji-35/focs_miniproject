
type var = string

type expr =
  | Integer of int
  | Variable of var
  | Mult of expr * expr
  | Plus of expr * expr
  | Let of var * expr * expr

type stmt =
  | Assign of var * expr
  | Print of expr

type program = stmt list


module SymTab = Map.Make(String)
type env = int SymTab.t

let rec eval eta = function
  | Integer i -> i
  | Variable v -> SymTab.find v eta
  | Plus (e1,e2) -> eval eta e1 + eval eta e2
  | Mult (e1,e2) -> eval eta e1 * eval eta e2
  | Let(v,e1,e2) ->  let v1 =  eval eta e1 in
                     let eta' = SymTab.add v v1 eta in
                     eval eta' e2

let runsmt eta = function
  | Assign (v,exp) -> let v1 = eval eta exp in
                      SymTab.add v v1 eta
  | Print e -> let v1 = eval eta e in
                        (Printf.printf "%d" v1 ;eta)

let run prog = List.fold_left runsmt SymTab.empty prog
