(* Parser *)


let slice_str (str: string) (start: int) : string =
  if String.length str < start + 1 then ""
  else
    String.sub str start ((String.length str) - start)

let rm_whsp (str: string) : string = String.trim str

let rm_cmnts (str: string) : string =
  if String.starts_with ~prefix:"//" str  then ""
  else
    str

