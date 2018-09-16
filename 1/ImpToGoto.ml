module Imp = ImpAST
module Gto = GotoAST
open CommonAST

let (++) = Gto.(++)
  
let new_label =
  let cpt = ref 0 in
  fun () -> incr cpt; CommonAST.Lab (Printf.sprintf "_label_%i" !cpt)

let translate_location = function
  | Imp.Identifier(i) ->
     Gto.Identifier(i)

let rec translate_expression = function
  | Imp.UnaryOp(uop,e) ->
     Gto.UnaryOp(uop,(translate_expression e))
  | Imp.BinaryOp(bop,e1,e2) ->
     Gto.BinaryOp(
                    bop,
                    (translate_expression e1),
                    (translate_expression e2)
                  )
  | Imp.Literal(l) ->
     Gto.Literal(l)
  | Imp.Location(l) ->
     Gto.Location(translate_location l)

let rec translate_instruction = function
  | Imp.Print(e) ->
     Gto.Print(translate_expression e)
  | Imp.Set(l,e) ->
     Gto.Set((translate_location l),(translate_expression e))
  | Imp.Conditional(e,i1,i2) ->
     let lbl_then = new_label ()
     and lbl_end = new_label ()
     in
     (Gto.ConditionalGoto(lbl_then, translate_expression e)) ++
     (translate_instruction i2) ++
     (Gto.Goto(lbl_end)) ++
     (Gto.Label(lbl_then)) ++
     (translate_instruction i1) ++
     (Gto.Label(lbl_end))
  | Imp.Loop(e,i) ->
     let lbl_start = new_label ()
     and lbl_loop = new_label ()
     and lbl_end = new_label ()
     in
     (Gto.Label(lbl_start)) ++
     (Gto.ConditionalGoto(lbl_loop, translate_expression e)) ++
     (Gto.Goto(lbl_end)) ++
     (Gto.Label(lbl_loop)) ++
     (translate_instruction i) ++
     (Gto.Goto(lbl_start)) ++
     (Gto.Label(lbl_end))
  | Imp.Sequence(i1,i2) ->
     Gto.Sequence(translate_instruction i1, translate_instruction i2)
  | Imp.Nop ->
     Gto.Nop

let translate_program p = Gto.({
  main = translate_instruction Imp.(p.main);
  globals = Imp.(p.globals);
})
