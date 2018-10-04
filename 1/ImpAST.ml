open CommonAST
open Expression

type instruction =
  | Print       of expression
  | Set         of location   * expression
  | Conditional of expression * instruction * instruction
  | Loop        of expression * instruction
  | Sequence    of instruction * instruction
  | Break
  | Continue
  | Nop

type program = {
  main: instruction;
  globals: typ Symb_Tbl.t;
}
