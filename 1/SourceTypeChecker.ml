open CommonAST
open SourceLocalisedAST

(* returns the type of a location in the given context *)
let type_location context l =
  let (Identifier(Id(id))) = l in
  Symb_Tbl.find id context.identifier_types

(* type reçu -> type attendu *)
exception Type_error of typ * typ * (int * int)
    
(* 
   raise a type_error if the given expression does not has the given type 
   in the given context
*)  
let rec assert_exp_type context e expected_type =
  let typ_e = type_expression context e in
  if typ_e != expected_type
  then
    raise (Type_error(typ_e,expected_type,e.e_pos))

(*
  return the type of the given expression in the given context
  raise not_found if an identifier is unknown in the given context
  raise type_error on type error
*)
and type_expression context e =
    match e.expr with
    | Location(l) ->
       type_location context l
    | Literal(Int(_)) -> TypInt
    | Literal(Bool(_)) -> TypBool
    | UnaryOp(uop,ex) ->
       begin
	 match uop with
	 | Minus ->
	    assert_exp_type context ex TypInt; TypInt
	 | Not ->
	    assert_exp_type context ex TypBool; TypBool
       end
    | BinaryOp(bop, e1, e2) ->
	  match bop with
	  | Add | Sub| Mult| Div| Mod ->
	     assert_exp_type context e1 TypInt;
	     assert_exp_type context e2 TypInt;
	     TypInt
	  | Lt | Le | Gt | Ge ->
	     assert_exp_type context e1 TypInt;
	     assert_exp_type context e2 TypInt;
	     TypBool
	  | And | Or ->
	     assert_exp_type context e1 TypBool;
	     assert_exp_type context e2 TypBool;
	     TypBool
	  | Eq | Neq ->
	     let typ_e1 = type_expression context e1 in
	     assert_exp_type context e2 typ_e1;
	     typ_e1
	    
	    
let rec typecheck_instruction context i = match i.instr with
  | Print(e) ->
     assert_exp_type context e TypInt
  | Set(l,e) ->
     let type_l = type_location context l in
     assert_exp_type context e type_l
  | Conditional(e,i1,i2) ->
     assert_exp_type context e TypBool;
     typecheck_instruction context i1;
     typecheck_instruction context i2
  | Loop(e,i) ->
     assert_exp_type context e TypBool;
     typecheck_instruction context i
  | Sequence(i1,i2) ->
     typecheck_instruction context i1;
     typecheck_instruction context i2
  | Break -> ()
  | Continue -> ()
  | Nop -> ()
    
let extract_context p =
  { identifier_types = p.globals; }
    
let typecheck_program p =
  let type_context = extract_context p in
  typecheck_instruction type_context p.main;
    
