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
- At last, all the output strings will be written in a file **out.hack** line by line.

---

## Execution

To execute this, follow the steps

- clone the repository properly
- go to the dir ./assembler/
- run  $dune build
- run  $dune exec assembler --  {path to file}
> THIS HAS TO BE RUN FROM THE DIR  **./assembler/**  only
- The output will be generated in  ./assembler/test/out.hack.

---

## Limitations

- It only shows basic errors (not implemented fully)
- The output file is hardcoded as  ./assembler/test/out.hack
- The encoded string of bits is hardcoded.
- Other limitations which I may be unaware of.

---

---

# JACK VM

## Files and Processes

### library

  - **vmast.ml** -> This files defines all the types for the vm
  - **vmparser.ml** -> This is the parser of the vm project, it takes input a program string and gives output a program of AST list
  - **encode.ml** -> This is the main encoding file, it encodes all the vm commands to corresponding asm list of instructions
  - **utils.ml** -> This is the helper module for encode.ml, it contains the asm list of instructions for a given operation

### bin

  - **main.ml** -> This is the main file of the project, it uses the lib files to genrate output, it contains the logic for generating asm file, file handling, and using the assembler for getting corresponding binary file.

### process

- The content of the input file is taken as a string
- The string then is passed to Parser and then cleaned(got rid of whitespaces and comments) and then a list of AST is generated.
- The list of ast is then encoded recursively to generate corresponding asm list of instructions.
- Then it is written in the file **out.asm** line by line.
- Then the assembler project is used whose input is out.asm and output is out.hack.

---

## Execution

To execute this, follow the steps

- clone the repository properly
- go to the dir ./jackvm/
- run  $dune build
- run  $dune exec jackvm --  {path to file}
> THIS HAS TO BE RUN FROM THE DIR  **./jackvm/**  only
- The output will be generated in  ./jackvm/test/out.asm and ./jackvm/test/out.hack .

---

## Limitations

- It only shows basic errors (not implemented fully)
- Static segment is not implemented (so any operation involving static will give error)
- It can handle only **one file** (as of now).
- The file should conatin Sys.init function, else it may not run properly.
- The output file is hardcoded as  ./jack/test/out.asm
- The encoded list of string of asm instructions is hardcoded.
- Other limitations which I may be unaware of.

---
