open CommonAST
open Expression
  
type instruction =
  | Print           of expression * instruction
  | Set             of location * expression * instruction
  (* CG(l,jump_instr,next_instr) *)
  | ConditionalGoto of expression * instruction * instruction
  | Nop

type program = {
  main: instruction;
  globals: typ Symb_Tbl.t;
}
