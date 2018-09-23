module Imp = ImpAST
module Gto = GotoAST
open CommonAST

exception Break_outside_of_loop
exception Continue_outside_of_loop

let (++) = Gto.(++)
  
let new_label =
  let cpt = ref 0 in
  fun () ->
    incr cpt; CommonAST.Lab (Printf.sprintf "_label_%i" !cpt)

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

(*
  May rise exceptions if a break/continue occurs
  out of any loop
*)
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
     (translate_instruction_in_loop lbl_start lbl_end i) ++
     (Gto.Goto(lbl_start)) ++
     (Gto.Label(lbl_end))
  | Imp.Sequence(i1,i2) ->
     Gto.Sequence(translate_instruction i1,
                  translate_instruction i2)
  | Imp.Break ->
     raise Break_outside_of_loop
  | Imp.Continue ->
     raise Continue_outside_of_loop
  | Imp.Nop ->
     Gto.Nop
(*
  The labels given are used to translate continue (lbl_start)
  and break (lbl_end) instructions
*)
and translate_instruction_in_loop lbl_start lbl_end = function
  | Imp.Break ->
     Gto.Goto(lbl_end)
  | Imp.Continue ->
     Gto.Goto(lbl_start)
  | Imp.Conditional(e,i1,i2) ->
     let lbl_then = new_label ()
     and lbl_end_if = new_label ()
     in
     (Gto.ConditionalGoto(lbl_then, translate_expression e)) ++
     (translate_instruction_in_loop lbl_start lbl_end i2) ++
     (Gto.Goto(lbl_end_if)) ++
     (Gto.Label(lbl_then)) ++
     (translate_instruction_in_loop lbl_start lbl_end i1) ++
     (Gto.Label(lbl_end_if))
  | Imp.Sequence(i1,i2) ->
     Gto.Sequence((translate_instruction_in_loop
                    lbl_start
                    lbl_end
                    i1),
                  (translate_instruction_in_loop
                    lbl_start 
                    lbl_end
                    i2)
                  )
  | i ->
     translate_instruction i

let translate_program p = Gto.({
  main = translate_instruction Imp.(p.main);
  globals = Imp.(p.globals);
})
