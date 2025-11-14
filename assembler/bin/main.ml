(*
   Main file for the whole project - Assembler
*)

open Lib
open Ast

(*This function calls the parser for parsing the input string to AST*)
let parse_file (filepath : string) : 'v Ast.Program.program =
  let in_chan = In_channel.open_text filepath in
  let program = In_channel.input_all in_chan in
  let instructions = Parser.parse_program program in
  In_channel.close in_chan;
  instructions

(*This function writes the output genereated into the output file*)
let write_lines (filepath : string) (lines: string list) : bool =
  let out_chan = Out_channel.open_text filepath in
  let rec write = function
  | [] -> ()
  | h :: t-> Out_channel.output_string out_chan (h ^ "\n");
    write t
  in
  write lines;
  Out_channel.close out_chan;
  true
  
    
(*This is a simple function just to convert the the C Instruction list of bits into a string*)
let string_of_intList (bits: int list) : string =
  List.map string_of_int bits |> String.concat ""

(*
   Main Function
*)
let () =
  (*Checks for correct number of input files*)
  match Array.length Sys.argv with
  | 2 ->
    let in_file = Sys.argv.(1) in
    let out_file = "./test/out.hack" in

    Printf.printf "Assembling %s -> %s\n" in_file out_file;

    let all_instructions = parse_file in_file in

    (*Calling the function to create symbol table with initially populated values*)
    let table = SymbolTable.create 100 in
    SymbolTable.populate_initial table;


    (*

       First Pass:
       The labels will be added in the symbol table with their corresponding rom address

       *)
    let first_pass instructions =
      let rec passing instructions rom_address =
        match instructions with
        |[] -> ()
        | (Inst.Label l_name) :: t-> SymbolTable.add table l_name rom_address;
                                     passing t rom_address
        | _ :: t ->  passing t (rom_address + 1)
      in

      passing instructions 0
    in
    first_pass all_instructions;

    (*

       Second Pass:
       This is the main pass which will make the final binary code list
       It will check for the instructions and if it is l instruction, it will skip that
       For A instruction of symbol , it will check if it is contained in the symbol table,
       If not, then it will add it corresponding to the current ram address in the symbol Table

    *)

    let rec binary_code inst ram_address =
      match inst with
      | [] -> []
      | h :: t->
        match h with
        | Inst.A symbol -> 
            if (SymbolTable.contains table symbol) then
              (Machine.Ainst.encode symbol table) :: binary_code t ram_address
            else
              begin
              SymbolTable.add table symbol ram_address;
              (Machine.Ainst.encode symbol table) :: binary_code t (ram_address + 1)
            end
        | Inst.AN value -> (Machine.Ainst.encodeN value) :: binary_code t ram_address
        | Inst.C c_record -> (Machine.Cinst.encode c_record |> string_of_intList) :: binary_code t ram_address
        | Inst.Label _ -> binary_code t ram_address
    in
    
    if write_lines out_file (binary_code all_instructions 16) then
      Printf.printf "Done assembling.\n"
    else
      Printf.eprintf "Error in writing"

   (*For the case when count of input files is not one*)
  | _ -> Printf.eprintf "error filename";
    exit 1

