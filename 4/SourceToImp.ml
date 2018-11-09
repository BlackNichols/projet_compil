module Src = SourceLocalisedAST
module Imp = ImpAST
open CommonAST

(* Fonction auxiliaire qui sera utilisée pour traduire les noms de champs en
   indices : le premier champ correspondra à l'indice [0], le deuxième à
   l'indice [1], et ainsi de suite.
   Type :
     assoc_index: 'a -> ('a * 'b) list -> int
   Déclenche l'exception [Not_found] si le champ cherché n'est pas présent. *)
let assoc_index k l =
  (* Fonction auxiliaire définissant la boucle de recherche. Le paramètre [i]
     est un compteur. *)
  let rec search i = function
    | [] -> raise Not_found
    | (a,_)::l when a=k -> i
    | c::l -> search (i+1) l
  in
  search 0 l


(* Pour traiter les opérations concernant les structures on aura besoin du
   contexte de typage. On le passe donc en paramètre, à redonner à chaque 
   appel récursif. *)
let rec strip_expression type_context e = match Src.(e.expr) with
  | Src.Literal lit ->
    Imp.Literal lit
      
  | Src.Location loc ->
    Imp.Location (strip_location type_context loc)
      
  | Src.UnaryOp(op, e) ->
    Imp.UnaryOp(op, strip_expression type_context e)
      
  | Src.BinaryOp(op, e1, e2) ->
    Imp.BinaryOp(op, strip_expression type_context e1, strip_expression type_context e2)

  (* Expression de création d'un tableau : rien de spécial.
     L'expression donnant la taille du tableau est propagée comme instruction
     donnant la taille du bloc et le paramètre donnant le type des éléments est
     ignoré. *)
  | Src.NewArray(e, _) ->
    Imp.NewBlock(strip_expression type_context e)

  (* Expression de création d'une structure : on a besoin de connaître sa
     taille. *)
  | Src.NewRecord(name) ->
    (* Récupération du type de la structure dont le nom est [name]. *)
    let struct_type = Symb_Tbl.find name type_context.struct_types in
    (* Le nombre de champs donne la taille du bloc à créer. *)
    let size = List.length struct_type.fields in
    (* Pour utiliser [NewBlock] on a besoin de fournir la taille sous la
       forme d'une expression du langage intermédiaire [ImpAST]. *)
    Imp.NewBlock(Imp.Literal(Int(size)))

(* Traduction des emplacements mémoire, avec le paramètre supplémentaire. *)
and strip_location type_context = function
  | Src.Identifier id ->
    Imp.Identifier id

  (* Cas de l'accès à un tableau : rien de spécial. *)
  | Src.ArrayAccess(e1, e2) ->
    Imp.BlockAccess(strip_expression type_context e1,
                    strip_expression type_context e2)

  (* Cas de l'accès à un champ d'une structure : on a besoin de connaître
     la place de ce champ dans la structure. *)
  | Src.FieldAccess(e, field_name) ->
    (* Comme différentes structures peuvent avoir chacune un champ [field_name]
       à une position différente, il faut connaître le type de la structure [e]
       considérée ici. *)
    let e_type = SourceTypeChecker.type_expression type_context e in
    (* Vérification de la forme du type de [e]. *)
    let struct_name = match e_type with
      (* S'il s'agit d'une structure tout va bien, on récupère son nom. *)
      | TypStruct(name) -> name
      (* Sinon erreur. *)
      | _ -> raise (SourceTypeChecker.Struct_type_expected(e_type, Src.(e.e_pos)))
    in
    (* Récupération de la liste des champs de la structure. *)
    let fields = (Symb_Tbl.find struct_name type_context.struct_types).fields in
    (* La position du champs dans la liste est retenue comme position dans la
       structure. *)
    let index = assoc_index field_name fields in
    (* Résultat : accès à un bloc, avec une expression donnant le numéro du
       champs. *)
    Imp.BlockAccess(strip_expression type_context e, Imp.Literal(Int index))

(* Traduction des instructions, avec le paramètre supplémentaire. *)
let rec strip_instruction type_context i = match Src.(i.instr) with
  | Src.Print e ->
    Imp.Print (strip_expression type_context e)
      
  | Src.Set(loc, e) ->
    Imp.Set(strip_location type_context loc, strip_expression type_context e)
      
  | Src.Conditional(e, i1, i2) ->
    Imp.Conditional(strip_expression type_context e,
                    strip_instruction type_context i1,
                    strip_instruction type_context i2)

  | Src.Loop(e, i) ->
    Imp.Loop(strip_expression type_context e, strip_instruction type_context i)
      
  | Src.Sequence(i1, i2) ->
    Imp.Sequence(strip_instruction type_context i1,
                 strip_instruction type_context i2)
      
  | Src.Nop ->
    Imp.Nop

let strip_program p =
  (* Préalablement à la traduction, on récupère le context de typage. *)
  let type_context = SourceTypeChecker.extract_context p in
  let main = strip_instruction type_context Src.(p.main) in
  let globals = Src.(p.globals) in
  (* Rappel : la table des structures n'est pas conservée dans la 
     représentation intermédiaire [ImpAST]. *)
  Imp.({ main; globals; })

(* Note de style : il est laborieux de redonner à chaque appel de
   [strip_expression], [strip_location] ou [strip_instruction] le nouveau
   paramètre [type_context], d'autant plus que la valeur de ce paramètre ne
   change jamais. On pourrait à la place le définir une fois pour toutes puis
   conserver les formes précédentes pour les appels à ces trois fonctions.

   Pour cela, définir les trois fonctions dans un contexte dans lequel
   [type_context] est déjà présent. Par exemple, les définir comme fonctions
   internes d'une fonction principale englobante. Cela donnerait la forme
   suivante :

   let strip_instruction type_context i =
     let rec strip_expression e = ...
       (* Recopier le code précédent, avec accès possible à [type_context]. *)
     and strip_location l = ...
       (* Recopier le code précédent, avec accès possible à [type_context]. *)
     and strip_instruction i = ...
       (* Recopier le code précédent, avec accès possible à [type_context]. *)
     in
     strip_instruction i
*)
