module Imp = ImpAST
module Gto = GotoAST
open CommonAST

let (++) = Gto.(++)

let rec translate_expression = function
  | Imp.Literal lit ->
    Gto.Literal lit

  | Imp.Location loc ->
    Gto.Location(translate_location loc)

  | Imp.UnaryOp(op, e) ->
    Gto.UnaryOp(op, translate_expression e)

  | Imp.BinaryOp(op, e1, e2) ->
    Gto.BinaryOp(op, translate_expression e1, translate_expression e2)

  (* Création d'un bloc : rien de spécial. *)
  | Imp.NewBlock(e) ->
     Gto.NewBlock(translate_expression e)
  | Imp.FunCall(id,args) ->
     Gto.FunCall(id, (List.map translate_expression args))
      
and translate_location = function
  | Imp.Identifier id ->
    Gto.Identifier id

  (* Accès à un bloc : rien de spécial. *)
  | Imp.BlockAccess(e1, e2) ->
    Gto.BlockAccess(translate_expression e1, translate_expression e2)


let new_label =
  let cpt = ref 0 in
  fun () -> incr cpt; CommonAST.Lab (Printf.sprintf "_label_%i" !cpt)

let rec translate_instruction = function
  | Imp.Sequence(i1, i2) ->
    Gto.Sequence(translate_instruction i1,
                 translate_instruction i2)
      
  | Imp.Print(e) ->
    Gto.Print(translate_expression e)
      
  | Imp.Set(loc, e) ->
    Gto.Set(translate_location loc, translate_expression e)

  | Imp.Conditional(c, i1, i2) ->
    let then_label = new_label()
    and end_label = new_label()
    in
    Gto.ConditionalGoto(then_label, translate_expression c)
    ++ translate_instruction i2
    ++ Gto.Goto end_label
    ++ Gto.Label then_label
    ++ translate_instruction i1
    ++ Gto.Label end_label

  | Imp.Loop(c, i) ->
    let test_label = new_label()
    and code_label = new_label()
    in
    Gto.Goto test_label
    ++ Gto.Label code_label
    ++ translate_instruction i
    ++ Gto.Label test_label
    ++ Gto.ConditionalGoto(code_label, translate_expression c)

  | Imp.Nop ->
    Gto.Nop

let translate_program p = Gto.({
  main = translate_instruction Imp.(p.main);
  globals = Imp.(p.globals);
})
