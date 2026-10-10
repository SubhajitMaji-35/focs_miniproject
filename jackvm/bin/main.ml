open Jackvm
open Vmast

(*Helper function to check if the given file has extension ".vm"*)
let check_vm_file (filename: string) : bool =
  Filename.check_suffix filename ".vm"

(*This function parses the whole program string and converts it into a list of ASTs*)
let parse_file (filepath: string) : 'v Program.t =
  let in_chan = In_channel.open_text filepath in
  let program = In_channel.input_all in_chan in
  let commands = Vmparser.parse_program program in
  In_channel.close in_chan;
  commands

(*Helper function to get the name of a directory, also for "." and ".."*)
let dir_name (path: string) : string =
  match Filename.basename path with
  | "." -> Filename.basename (Sys.getcwd ())
  | ".." -> Filename.basename (Filename.dirname (Sys.getcwd ()))
  | name -> name

(*
   This function gives the list of vm files to translate and the output asm file
   Foo.vm -> [Foo.vm] and Foo.asm
   a directory Dir -> all the .vm files in Dir and Dir/Dir.asm
*)
let get_files (path: string) : string list * string =
  if not (Sys.file_exists path) then failwith ("No such file or directory: " ^ path)
  else if Sys.is_directory path then
    match Sys.readdir path |> Array.to_list |> List.filter check_vm_file |> List.sort compare with
    | [] -> failwith ("No .vm files in the directory " ^ path)
    | files -> (List.map (Filename.concat path) files, Filename.concat path (dir_name path ^ ".asm"))
  else if check_vm_file path then ([path], Filename.remove_extension path ^ ".asm")
  else failwith "Expected a file with .vm extension or a directory"

(*Helper function to check if a command defines Sys.init*)
let is_sys_init = function
  | Command.C_Function ("Sys.init", _) -> true
  | _ -> false

(*
   This function translates the ASTs of all the files to corresponding asm instructions
   The bootstrap code is added only if some file defines Sys.init
*)
let get_asm (files: string list) : string list =
  let programs = List.map (fun file -> (Filename.basename file |> Filename.remove_extension, parse_file file)) files in
  let asm = List.concat_map (fun (name, commands) -> Encode.encode name commands) programs in
  if List.exists (fun (_, commands) -> List.exists is_sys_init commands) programs then
    Utils.bootstrap @ asm
  else asm

(*This function writes the given asm instructions line by line into the output file*)
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
    let out_file, asm_codes =
      try
        let files, out_file = get_files in_file in
        (out_file, get_asm files)
      with Failure msg | Sys_error msg ->
        Printf.eprintf "Error: %s\n" msg;
        exit 1
    in

    Printf.printf "Translating %s -> %s\n%!" in_file out_file;

    if write_lines out_file asm_codes then
      Printf.printf "Done translating.\n%!"
    else
      Printf.eprintf "Error in writing%!";

    (*After translating the vm codes to asm codes, the asm codes are now translated to hack binary using the assembler project*)
    (*The assembler is run from its own project dir, so the path of the asm file is made absolute*)
    let out_file = if Filename.is_relative out_file then Filename.concat (Sys.getcwd ()) out_file else out_file in
    (match Sys.command (Printf.sprintf "cd ../assembler && dune exec assembler -- %s" (Filename.quote out_file)) with
     | 0 -> ()
     | _ -> Printf.eprintf "Error: assembling %s failed\n" out_file; exit 1)

  | _ -> Printf.eprintf "Error filename";
    exit 1


