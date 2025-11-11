## SUBHAJIT MAJI - 112401035

---

# HACK ASSEMBLER

## Overview

- This Project is developed as a part of **CS2111 - Foundations of Computing Systems**.
- A complete assembler implemented in OCaml , based on the **Nand2Tetris** course.
- This project translates a hack assembly file(.asm) to corresponding Binary Machine code (.hack).

---

## Files and Processes

### library

- **ast.ml** -> This file defines all the types
- **symbolTable.ml** -> This file contains all the functions to create a symbol table, inititalize it and methods to add, contains (check if it contains), find
- **machine.ml** -> This file contains the main encoding of a program of AST list, it encodes the A-instructions and C-instructions to their corresponding binary codes
- **parser.ml** -> This is the parser of the project, it takes a input program string and gives output a program of AST lists
- **mnemonics.ml** -> This contains some common mnemonics, which may ease some part of the codes, but as of now, I have not utilised it in any part.

### bin
 - **main.ml** -> This is the main file of the whole project, it uses all the functions defined in the lib dir, and contains logic for first pass and second pass and the file handling.

### process

- The content of the input file is taken as a string.
- The string then is passed to Parser and then cleaned(got rid of whitespaces and comments) and then a list of AST is generated.
- A symbol table is generated with initialization.
- The list of AST is then is then iterated in the Main for first pass, the labels are added in the symbol table and removed.
- In the second pass, all the A instructions and C instructions are encoded using the Machine module.
- At last, all the output strings will be written in a file *out.hack* line by line.

---

## Execution

To execute this, follow the steps

- clone the repository properly
- go to the dir ./assembler/
- run  $dune build
- run  $dune exec assembler --  {path to file}
- The output will be generated in  ./assembler/test/out.asm.

---

## Limitations

- It only shows basic errors (not implemented fully)
- The output file is hardcoded as  ./assembler/test/out.hack
- The encoded string of bits is hardcoded.
- Other limitations which I may be unaware of.

  ---
