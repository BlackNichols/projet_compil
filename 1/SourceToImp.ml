module Src = SourceLocalisedAST
module Imp = ImpAST
module Expr = Expression
open CommonAST

let rec strip_expression e = match Src.(e.expr) with
  | Src.UnaryOp(uop,e) ->
     Expr.UnaryOp(uop, strip_expression e)
  | Src.BinaryOp(bop,e1,e2) ->
     Expr.BinaryOp(bop, strip_expression e1, strip_expression e2)
  | Src.Literal(l) ->
     Expr.Literal(l)
  | Src.Location(Src.Identifier(i)) ->
     Expr.Location(Expr.Identifier(i))

let rec strip_instruction i = match Src.(i.instr) with
  | Src.Nop ->
     Imp.Nop
  | Src.Print(e) ->
     Imp.Print(strip_expression e)
  | Src.Set(Src.Identifier(i),e) ->
     Imp.Set(Expr.Identifier(i), strip_expression e)
  | Src.Conditional(e,i1,i2) ->
     Imp.Conditional(strip_expression e,
		     strip_instruction i1,
		     strip_instruction i2)
  | Src.Loop(e,i) ->
     Imp.Loop(strip_expression e, strip_instruction i)
  | Src.Sequence(i1,i2) ->
     Imp.Sequence(strip_instruction i1, strip_instruction i2)
  | Src.Break ->
     Imp.Break
  | Src.Continue ->
     Imp.Continue
      
let strip_program p =
  let main = strip_instruction Src.(p.main) in
  let globals = Src.(p.globals) in
  Imp.({ main; globals; })
