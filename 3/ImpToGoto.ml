module Imp = ImpAST
module Gto = GotoAST
open CommonAST

let (++) = Gto.(++)

(* Traduction des expressions à l'identique, avec descente récursive dans
   les sous-expressions. *)
let rec translate_expression = function
  | Imp.Literal lit ->
    Gto.Literal lit

  | Imp.Location loc ->
    Gto.Location(translate_location loc)

  | Imp.UnaryOp(op, e) ->
    Gto.UnaryOp(op, translate_expression e)

  | Imp.BinaryOp(op, e1, e2) ->
    Gto.BinaryOp(op, translate_expression e1, translate_expression e2)

(* Traduction directe des emplacements mémoire *)
and translate_location = function
  | Imp.Identifier id ->
    Gto.Identifier id

(* Création d'une nouvelle étiquette *)
let new_label =
  let cpt = ref 0 in
  fun () -> incr cpt; CommonAST.Lab (Printf.sprintf "_label_%i" !cpt)

(* Traduction des instructions *)
let rec translate_instruction = function
  (* Les cas de la séquence, de l'affichage et de l'affectation sont 
     traduits à l'identique, avec descente récursive dans les expressions
     et sous-instructions. *)
  | Imp.Sequence(i1, i2) ->
    Gto.Sequence(translate_instruction i1,
                 translate_instruction i2)
      
  | Imp.Print(e) ->
    Gto.Print(translate_expression e)
      
  | Imp.Set(loc, e) ->
    Gto.Set(translate_location loc, translate_expression e)

  (* Branchement conditionnel. *)
  | Imp.Conditional(c, i1, i2) ->
    (* Création d'étiquettes pour le début du bloc [then] et pour le point
       de jonction à la fin de l'instruction. *)
    let then_label = new_label()
    and end_label = new_label()
    in
    (* Si la garde s'évalue à [true], aller au bloc [then] *)
    Gto.ConditionalGoto(then_label, translate_expression c)
    (* Sinon exécuter le bloc [else] *)
    ++ translate_instruction i2
    (* À la fin du bloc [else], sauter au point de jonction *)
    ++ Gto.Goto end_label
    (* Bloc [then] *)
    ++ Gto.Label then_label
    ++ translate_instruction i1
    (* À la fin du bloc [then], poursuivre vers le point de jonction *)
    (* Point de jonction *)
    ++ Gto.Label end_label

  (* Boucle conditionnelle *)
  | Imp.Loop(c, i) ->
    (* Création d'étiquettes pour le test et le corps de la boucle *)
    let test_label = new_label()
    and code_label = new_label()
    in
    (* Aller au test (dont on a placé le code à la fin) *)
    Gto.Goto test_label
    (* Corps de la boucle *)
    ++ Gto.Label code_label
    ++ translate_instruction i
    (* À la fin de l'exécution du corps de la boucle, poursuivre vers le test *)
    (* Test *)
    ++ Gto.Label test_label
    (* Si la garde s'évalue à [true], revenir au corps de la boucle *)
    ++ Gto.ConditionalGoto(code_label, translate_expression c)
    (* Sinon, fin *)

  | Imp.Nop ->
    Gto.Nop

(* Traduction des programmes *)
let translate_program p = Gto.({
  main = translate_instruction Imp.(p.main);
  globals = Imp.(p.globals);
})
