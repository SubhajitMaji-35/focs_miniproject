{
open Parser
}

let newline =['\n' '\r']+
let whitespace = [' ' '\t']+
let comment = "//"[^ '\n' '\r']*

let symbol = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '$' '.' ':']*

let number = ['0'-'9']+

rule read =
    parse

    |'@' { AT }
    |'=' { EQ }
    |';' { SEMI }
    |'(' { LPAREN }
    |')' { RPAREN }
    |'+' { PLUS }
    |'-' { MINUS }
    |'&' { AND }
    |'|' { OR }
    |'!' { NOT }

    |whitespace { read lexbuf }
    |comment { read lexbuf }
    |newline { Lexing.new_line lexbuf;NEWLINE }

    |"0" { ZERO }
    |"1" { ONE }

    |"A" { A }
    |"D" { D }
    |"M" { M }

    |"JGT" { JGT }
    |"JGE" { JGE }
    |"JEQ" { JEQ }
    |"JNE" { JNE }
    |"JLE" { JLE }
    |"JLT" { JLT }
    |"JMP" { JMP }

    | symbol { SYMBOL (Lexing.lexeme lexbuf) }
    | number { NUMBER (int_of_string(Lexing.lexeme lexbuf)) }

    | eof { EOF }
    | _ as c { failwith ("Error in Lexer : " ^ (String.make 1 c)) }

