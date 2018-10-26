module Src = SourceLocalisedAST
module Imp = ImpAST
open CommonAST

(* Traduction d'une expression.
   Dans la structure { expr ; e_pos } on récupère l'expression elle-même
   et on la reproduit à l'identique, en descendant récursivement dans les
   sous-expressions. *)
let rec strip_expression e = match Src.(e.expr) with
  | Src.Literal lit ->
    Imp.Literal lit
      
  | Src.Location loc ->
    Imp.Location (strip_location loc)
      
  | Src.UnaryOp(op, e) ->
    Imp.UnaryOp(op, strip_expression e)
      
  | Src.BinaryOp(op, e1, e2) ->
    Imp.BinaryOp(op, strip_expression e1, strip_expression e2)

(* Traduction directe des emplacements mémoire. *)
and strip_location = function
  | Src.Identifier id ->
    Imp.Identifier id

(* Traduction des instructions.
   Dans la structure { instr ; i_pos } on récupère l'instruction elle-même
   et on la reproduit à l'identique, en descendant récursivement dans les
   expressions et les sous-instructions. *)
let rec strip_instruction i = match Src.(i.instr) with
  | Src.Print e ->
    Imp.Print (strip_expression e)
      
  | Src.Set(loc, e) ->
    Imp.Set(strip_location loc, strip_expression e)
      
  | Src.Conditional(e, i1, i2) ->
    Imp.Conditional(strip_expression e,
                    strip_instruction i1, strip_instruction i2)

  | Src.Loop(e, i) ->
    Imp.Loop(strip_expression e, strip_instruction i)
      
  | Src.Sequence(i1, i2) ->
    Imp.Sequence(strip_instruction i1, strip_instruction i2)
      
  | Src.Nop ->
    Imp.Nop

(* Traduction d'un programme. *)
let strip_program p =
  let main = strip_instruction Src.(p.main) in
  let globals = Src.(p.globals) in
  Imp.({ main; globals; })
