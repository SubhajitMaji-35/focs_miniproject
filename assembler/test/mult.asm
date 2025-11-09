// R2 = R0 * R1  (R1 will become 0)
@2
M=0        // R2 = 0 (accumulator)

(LOOP)
@1
D=M
@END
D;JEQ      // if R1 == 0 goto END

@0
D=M        // D = R0
@2
M=D+M      // R2 = R2 + R0

@1
M=M-1      // R1 = R1 - 1
@LOOP
0;JMP

(END)
@END
0;JMP

