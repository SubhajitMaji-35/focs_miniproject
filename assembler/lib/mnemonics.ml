
open Ast

module Jump = struct
let jumpof (j:Inst.jinst) : Inst.cinst =
  match j with
  | JMP -> {destination= [];
            output= Inst.Constant Zero;
            jump = Some JMP}
  | _ -> {destination = [];
          output = Inst.Uapply(ID, D);
          jump = Some j}

let jgt = jumpof JGT

let jeq = jumpof JEQ

let jge = jumpof JGE

let jlt = jumpof JLT

let jne = jumpof JNE

let jle = jumpof JLE

let jmp = jumpof JMP

end

module OutOP = struct
  let add (r: Reg.t ) = Inst.Bapply(Add, r)

  let sub (r: Reg.t ) = Inst.Bapply(Sub, r)

  let subfrom (r: Reg.t ) = Inst.Bapply(SubFrom, r)

  let band (r: Reg.t ) = Inst.Bapply(BAnd, r)

  let bor (r: Reg.t ) = Inst.Bapply(BOr, r)

  let bneg (r: Reg.t ) = Inst.Uapply(BNeg, r)

  let succ (r: Reg.t) = Inst.Uapply(Succ, r)

  let pred (r:Reg.t) = Inst.Uapply(Pred, r)
    
end

let assign (r:Reg.t) (o: Inst.output) : 'v Inst.t = C {destination = [r];
                                                         output = o;
                                                         jump = None}


