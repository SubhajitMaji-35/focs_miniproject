(* Symbol Table Interface *)

type t = (string, int) Hashtbl.t
    
(**
 * Create a new, empty symbol table.
 * The int is the initial size (a good default is 128).
*)
    
val create : int -> t

(**
 * Add all predefined symbols (R0-R15, SP, LCL, etc.)
 * to the given table. This *mutates* the table.
 *)
val populate_initial : t -> unit

(**
 * Add a new symbol-to-address mapping to the table.
 * This *mutates* the table.
 *)
val add : t -> string -> int -> unit

(**
 * Find the address for a given symbol.
 * Returns None if the symbol is not in the table.
 *)
val find : t -> string -> int option

(**
 * Check if a symbol exists in the table.
 *)
val contains : t -> string -> bool
