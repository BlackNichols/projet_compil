open CommonAST
open SourceLocalisedAST

(* Erreur de type.
   Paramètres : type trouvé, type attendu, position. *)
exception Type_error of typ * typ * (int * int)
(* ArrayAccess sur une variable qui n'est pas un tableau 
   Paramètres : type trouvé * position *)
exception Array_type_error of typ * (int * int)

(* Fonction auxiliaire vérifiant le bon typage de l'expression [e]
   et comparant son type avec le type attendu [expected_type]. *)
let rec check_type context e expected_type =
  (* Typage de [e] *)
  let e_type = type_expression context e in
  if e_type = expected_type
  (* Si le type est celui attendu, alors ne rien faire *)
  then ()
  (* Sinon erreur *)
  else raise (Type_error(e_type, expected_type, e.e_pos))

(* Typage des expressions *)
and type_expression context e = match e.expr with
  (* Constantes et emplacements mémoire : fonctions auxiliaires *)
  | Literal lit -> type_literal lit
  | Location loc -> type_location context e.e_pos loc 

  (* L'opérateur [-] unaire attend un entier et produit un entier *)
  | UnaryOp(Minus, e) -> check_type context e TypInt; TypInt
  (* L'opérateur [!] attend un booléen et produit un booléen *)
  | UnaryOp(Not, e) -> check_type context e TypBool; TypBool
                         
  | BinaryOp(op, e1, e2) ->
    (* Séparation des opérateurs binaires en quatre profils *)
    let operand_type, result_type = match op with
      (* Opérateurs arithmétiques : opérandes entiers, résultat entiers *)
      | Add | Sub | Mult | Div | Mod -> TypInt, TypInt
      (* Comparaisons : opérandes entiers, résultat booléen *)
      | Lt | Le | Gt | Ge -> TypInt, TypBool
      (* Opérateurs logiques : opérandes booléens, résultat booléen *)
      | And | Or -> TypBool, TypBool
      (* Tests d'égalité : on prend comme type attendu des opérandes le type
         du premier opérande, résultat booléen *)
      | Eq | Neq -> type_expression context e1, TypBool
    in
    check_type context e1 operand_type;
    check_type context e2 operand_type;
    result_type
  | NewArray(t,e) -> check_type context e TypInt; TypArray(t)

(* Typage des constantes *)
and type_literal = function
  | Int _ -> TypInt
  | Bool _ -> TypBool

(* Typage des emplacements mémoire *)

and type_location context pos = function
  (* La table des symboles indique le type associé à une variable *)
  | Identifier(Id id) -> Symb_Tbl.find id context.identifier_types
  | ArrayAccess(l,e) ->
     check_type context e TypInt;
    let t = type_location context pos l in
    begin
      match t with
      | TypArray(t2) -> t2
      | _ -> raise (Array_type_error(t,pos))
    end

(* Vérification de la cohérence des instructions *)
let rec typecheck_instruction context i = match i.instr with
  (* L'instruction [print] attend un paramètre entier *)
  | Print e -> check_type context e TypInt

  (* Pour une affectation, le type attendu de l'expression est le type
     associé à l'emplacement mémoire *)
  | Set(loc, e) ->
    let loc_type = type_location context i.i_pos loc in
    check_type context e loc_type

  (* La garde d'un branchement conditionnel doit être booléenne *)
  | Conditional(e, i1, i2) ->
    check_type context e TypBool;
    typecheck_instruction context i1;
    typecheck_instruction context i2

  (* La garde d'une boucle conditionnelle doit être booléenne *)
  | Loop(e, i) ->
    check_type context e TypBool;
    typecheck_instruction context i
    
  | Sequence(i1, i2) ->
    typecheck_instruction context i1;
    typecheck_instruction context i2

  | Nop -> ()

(* Construction du contexte de typage.
   Le contexte est pour l'instant constitué de la seule table des symboles. *)
let extract_context p =
  { identifier_types = p.globals; }

(* Vérification de la cohérence d'un programme *)
let typecheck_program p =
  let type_context = extract_context p in
  typecheck_instruction type_context p.main;
    
