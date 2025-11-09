// Reverse array at base R0 of length R1

@R0
D=M
@BASE
M=D
@R1
D=M
@LEN
M=D

@START
M=0
@ENDP
M=M-1
@LEN
D=M
@ENDP
M=D-1     // ENDP = len - 1

(REVERSE_LOOP)
@START
D=M
@ENDP
D=D-M
@DONE
D;JGE     // if start >= end, done

// temp = A[start]
@BASE
D=M
@START
A=D+M
D=M
@TEMP
M=D

// A[start] = A[end]
@BASE
D=M
@ENDP
A=D+M
D=M
@BASE
A=M
@START
A=A+M
M=D

// A[end] = temp
@TEMP
D=M
@BASE
A=M
@ENDP
A=A+M
M=D

// start++
@START
M=M+1
// end--
@ENDP
M=M-1
@REVERSE_LOOP
0;JMP

(DONE)
@DONE
0;JMP

