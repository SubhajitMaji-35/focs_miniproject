// R2 = max value in array starting at R0 of length R1

@R0
D=M
@arr
M=D          // arr = base address
@R1
D=M
@len
M=D          // len = length

@arr
A=M
D=M
@R2
M=D          // R2 = first element

@i
M=1          // i = 1

(LOOP)
@i
D=M
@len
D=D-M
@END
D;JGE        // if i >= len, done

// D = arr[i]
@arr
D=M
@i
A=D+M
D=M          // D = array[i]

// if array[i] > R2, update max
@R2
D=D-M
@NOUPDATE
D;JLE        // skip if not greater

@arr
D=M
@i
A=D+M
D=M
@R2
M=D          // R2 = array[i]

(NOUPDATE)
// i++
@i
M=M+1
@LOOP
0;JMP

(END)
@END
0;JMP

