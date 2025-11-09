// R2 = max(R0, R1)
@0
D=M        // D = R0
@1
D=D-M      // D = R0 - R1
@R0_GREATER
D;JGT      // if R0 > R1 jump
// else R1 >= R0
@1
D=M
@2
M=D        // R2 = R1
@END
0;JMP

(R0_GREATER)
@0
D=M
@2
M=D        // R2 = R0

(END)
@END
0;JMP

