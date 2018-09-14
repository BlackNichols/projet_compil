open CommonAST
open SourceLocalisedAST

(* returns the type of a location in the given context *)
let type_location context l =
  let (Identifier(Id(id))) = l in
  Symb_Tbl.find id context

(* type reçu -> type attendu *)
exception Type_error of typ * typ * (int * int)
    
(* 
   raise a type_error if the given expression does not has the given type 
   in the given context
*)  
let rec assert_type context e expected_type =
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
	    assert_type context ex TypInt; TypInt
	 | Not ->
	    assert_type context ex TypBool; TypBool
       end
    | BinaryOp(bop, e1, e2) ->
	  match bop with
	  | Add | Sub| Mult| Div| Mod ->
	     assert_type context e1 TypInt;
	     assert_type context e2 TypInt;
	     TypInt
	  | Lt | Le | Gt | Ge ->
	     assert_type context e1 TypInt;
	     assert_type context e2 TypInt;
	     TypBool
	  | And | Or ->
	     assert_type context e1 TypBool;
	     assert_type context e2 TypBool;
	     TypBool
	  | Eq | Neq ->
	     let typ_e1 = type_expression context e1 in
	     assert_type context e2 typ_e1;
	     typ_e1
	    
	    
let rec typecheck_instruction context i = match i.instr with
  | Print(e) ->
     assert_type context e TypInt
  | Set(l,e) -> ()
     
  | Nop -> ()

  | _ -> failwith "Not implemented"
    
let extract_context p =
  { identifier_types = p.globals; }
    
let typecheck_program p =
  let type_context = extract_context p in
  typecheck_instruction type_context p.main;
    
