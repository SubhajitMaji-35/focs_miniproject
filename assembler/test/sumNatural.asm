// Computes sum = 1 + 2 + ... + R0
// Result → R1

@0
D=M        // D = N
@N
M=D        // store N for clarity

@1
M=0        // R1 = 0 (sum = 0)
@i
M=1        // i = 1

(LOOP)
@i
D=M
@N
D=D-M
@END
D;JGT      // if i > N, jump END

// sum = sum + i
@i
D=M
@1
M=D+M

// i = i + 1
@i
M=M+1
@LOOP
0;JMP

(END)
@END
0;JMP

