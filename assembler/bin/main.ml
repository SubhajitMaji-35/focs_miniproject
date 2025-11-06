open Lib
open Ast

let parse_file (filepath : string) : 'v Ast.Program.program =
  let in_chan = In_channel.open_text filepath in
  let program = In_channel.input_all in_chan in
  let instructions = Parser.parse_program program in
  In_channel.close in_chan;
  instructions

let string_of_intList (bits: int list) : string =
  List.map string_of_int bits |> String.concat ""

let () =
  match Array.length Sys.argv with
  | 2 ->
    let in_file = Sys.argv.(1) in
    let out_file = "out.hack" in

    Printf.printf "Assembling %s -> %s\n" in_file out_file;

    let all_instructions = parse_file in_file in

    let table = SymbolTable.create 100 in
    SymbolTable.populate_initial table;

    let handle_inst table rom_address inst =
      match inst with
      | Inst.Label label_name ->
        SymbolTable.add table label_name !rom_address
      | _ -> rom_address := !rom_address + 1
    in

    let first_pass table all_instructions =
      let rom_address = ref 0 in
      List.iter (fun inst -> handle_inst table rom_address inst) all_instructions in
    first_pass table all_instructions;


    let ram_address = ref 16 in
    
    let binary_code = List.filter_map (fun inst ->
      match inst with
      | Inst.A symbol ->
        Some (Machine.Ainst.encode symbol table ram_address)
      | Inst.Ref value ->
        Some (Machine.Ainst.encode (string_of_int value) table ram_address)
      | Inst.C c_record ->
        Some (Machine.Cinst.encode c_record |> string_of_intList)
      | Inst.Label _ ->
        None
      ) all_instructions in

    let out_chan = Out_channel.open_text out_file in
    List.iter (fun line -> Out_channel.output_string out_chan (line ^ "\n")
              ) binary_code;
    Printf.printf "Done assembling.\n"
      
  | _ -> Printf.eprintf "error filename";
    exit 1

