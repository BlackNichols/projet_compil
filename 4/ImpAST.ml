open CommonAST

type expression =
  | Literal  of literal
  | Location of location
  | UnaryOp  of unaryOp  * expression
  | BinaryOp of binaryOp * expression * expression
  (* Création d'un bloc. *)
  | NewBlock of expression
  | FunCall of identifier * expression list

and location =
  | Identifier  of identifier
  (* Accès à un bloc. *)
  | BlockAccess of expression * expression

type instruction =
  | Print       of expression
  | Set         of location   * expression
  | Conditional of expression * instruction * instruction
  | Loop        of expression * instruction
  | Sequence    of instruction * instruction
  | Nop

type program = {
  main: instruction;
  globals: typ Symb_Tbl.t;
}
