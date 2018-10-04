open CommonAST
open Expression

(* ;-) *)
let labelTblsk = Hashtbl.create 13

type tmp_instruction =
  | Print           of expression * instruction
  | Set             of location * expression * instruction
  | Goto            of lbl
  | ConditionalGoto of expression * lbl * instruction
  | Nop

and lbl =
  | Label of label
  | Instruction of instruction
      
type program = {
  main: instruction;
  globals: typ Symb_Tbl.t;
}
