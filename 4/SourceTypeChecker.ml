open CommonAST
open SourceLocalisedAST

exception Type_error of typ * typ * (int * int)
exception Array_type_expected of typ * (int * int)
exception Struct_type_expected of typ * (int * int)
exception Unknown_function of identifier * (int * int)
exception Wrong_argument_number of identifier * (int * int)

let rec check_type context e expected_type =
  let e_type = type_expression context e in
  if e_type = expected_type
  then ()
  else raise (Type_error(e_type, expected_type, e.e_pos))

and type_expression context e = match e.expr with
  | Literal lit -> type_literal lit
  | Location loc -> type_location context loc 

  | UnaryOp(Minus, e) -> check_type context e TypInt; TypInt
  | UnaryOp(Not, e) -> check_type context e TypBool; TypBool
                         
  | BinaryOp(op, e1, e2) ->
    let operand_type, result_type = match op with
      | Add | Sub | Mult | Div | Mod -> TypInt, TypInt
      | Lt | Le | Gt | Ge -> TypInt, TypBool
      | And | Or -> TypBool, TypBool
      | Eq | Neq -> type_expression context e1, TypBool
    in
    check_type context e1 operand_type;
    check_type context e2 operand_type;
    result_type

  (* Typage de la création d'un tableau : l'expression donnant la taille doit
     être un entier. Le résultat est un type de tableau dont les éléments sont
     du type [ty] donné en paramètre. *)
  | NewArray(e, ty) ->
    check_type context e TypInt;
    TypArray(ty)

  (* Typage de la création d'une structure : on vérifie seulement que le nom
     existe dans le contexte de typage. *)
  | NewRecord(name) ->
    if Symb_Tbl.mem name context.struct_types
    then TypStruct(name)
    else failwith "Unknown struct"
  | FunCall(id,args) ->
     begin
       try
	 let sign = Symb_Tbl.find id context.function_signatures in
	 iter2
	   (fun (_ , t) e = check_type context e t)
	   sign.formals
	   args
       with
       | Not_found -> raise (Unknown_function(id,e.e_pos))
       | Invalid_argument -> raise (Wrong_argument_number(id,e.e_pos))
     end
       

and type_literal = function
  | Int _ -> TypInt
  | Bool _ -> TypBool

and type_location context = function
  | Identifier(Id id) -> Symb_Tbl.find id context.identifier_types

  (* Typage de l'accès à un tableau. *)
  | ArrayAccess(e1, e2) ->
    (* Calcul du type de l'expression [e1] désignant le tableau. *)
    let e1_type = type_expression context e1 in
    (* Vérification de la forme du type de [e1] *)
    let contents_type = match e1_type with
      (* S'il s'agit d'un type tableau tout va bien. On récupère le type des
         éléments du tableau. *)
      | TypArray t -> t
      (* Sinon erreur. *)
      | _ -> raise (Array_type_expected(e1_type, e1.e_pos))
    in
    (* Vérification du type de l'expression [e2] désignant l'indice entier. *)
    check_type context e2 TypInt;
    (* Résultat : type du contenu du tableau. *)
    contents_type

  (* Typage de l'accès à une structure. *)
  | FieldAccess(e, field_name) ->
    (* Calcul du type de l'expression [e] désignant la structure. *)
    let e_type = type_expression context e in
    (* Vérification de la forme du type de [e]. *)
    let fields = match e_type with
      (* S'il s'agit d'un type de structure tout va bien. On récupère la liste
         des champs et de leurs types. *)
      | TypStruct(name) -> (Symb_Tbl.find name context.struct_types).fields
      (* Sinon erreur. *)
      | _ -> raise (Struct_type_expected(e_type, e.e_pos))
    in
    (* Résultat : type du champ dont le nom est [field_name]. 
       Note : cette expression déclenchera l'exception [Not_found] si le champ
       n'existe pas. On pourrait utiliser en plus un [try/with] pour renvoyer
       dans ce cas un diagnostic plus précis. *)
    List.assoc field_name fields

let rec typecheck_instruction context i = match i.instr with
  | Print e -> check_type context e TypInt

  | Set(loc, e) ->
    let loc_type = type_location context loc in
    check_type context e loc_type

  | Conditional(e, i1, i2) ->
    check_type context e TypBool;
    typecheck_instruction context i1;
    typecheck_instruction context i2

  | Loop(e, i) ->
    check_type context e TypBool;
    typecheck_instruction context i
    
  | Sequence(i1, i2) ->
    typecheck_instruction context i1;
    typecheck_instruction context i2

  | Nop -> ()

let extract_context p =
  { identifier_types = p.globals;
    (* Le contexte de typage contient maintenant en plus la table des 
       structures. *)
    struct_types = p.structs; }

let typecheck_program p =
  let type_context = extract_context p in
  typecheck_instruction type_context p.main;
    
