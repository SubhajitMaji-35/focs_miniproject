open Jackvm
open Vmast

let check_vm_file (filename: string) : bool =
  Filename.check_suffix filename ".vm"

let parse_file (filepath: string) : 'v Program.t =
  let in_chan = In_channel.open_text filepath in
  let program = In_channel.input_all in_chan in
  let commands = Vmparser.parse_program program in
  In_channel.close in_chan;
  commands

let get_asm (commands: 'v Program.t) : string list =
  Utils.bootstrap ()
  @ Encode.encode commands

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

let () =
  match Array.length Sys.argv with
  | 2->
    let in_file = Sys.argv.(1) in
    let out_file = "./test/out.asm" in

    if not (check_vm_file in_file) then
      (Printf.eprintf "Error: Expected a file with .vm extension\n";
       exit 1);

    Printf.printf "Translating %s -> %s\n%!" in_file out_file;

    let commands = parse_file in_file in
    let asm_codes = get_asm commands in

    if write_lines out_file asm_codes then
      Printf.printf "Done translating.\n%!"
    else
      Printf.eprintf "Error in writing%!";

    let _ = Sys.command (Printf.sprintf "dune exec assembler -- %s" out_file ) in
    ()

  | _ -> Printf.eprintf "Error filename";
    exit 1

