(*Symbol Table*)

type t = (string, int) Hashtbl.t

let create (size : int) : t = Hashtbl.create size

let add = Hashtbl.add
let contains = Hashtbl.mem

let find = Hashtbl.find_opt

(*Helper function to add the initial set of 16 registers*)
let add_registers (table : t) : unit =
  let rec add_registers_rec (table : t) (i : int) : unit =
  if i > 15 then
    ()
  else
    let label = "R" ^ string_of_int i in
    add table label i;
    add_registers_rec table (i + 1)
  in
  add_registers_rec table 0


(*Function to populate the Symbol Table with the initial key value pairs*)
let populate_initial (table: t) : unit =
  add table "SP" 0;
  add table "LCL" 1;
  add table "ARG" 2;
  add table "THIS" 3;
  add table "THAT" 4;
  add table "SCREEN" 16384;
  add table "KBD" 24576;
  add_registers table

