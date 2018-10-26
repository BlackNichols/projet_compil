open CommonAST
open GotoAST
open Mips

let push reg = sw reg 0 sp  @@ subi sp sp 4
let pop  reg = addi sp sp 4 @@ lw reg 0 sp

(* Chargement d'une valeur immédiate dans le registre t0 *)
let translate_literal = function
  | Int i  -> li t0 i
  | Bool b -> li t0 (if b then -1 else 0)

(* Chargement de l'adresse d'une variable dans le registre t0 *)
let rec translate_location = function
  | Identifier (Id id) -> la t0 id

(* Calcul de la valeur d'une expression. La valeur résultat est placée
   au sommet de la pile. *)
and translate_expression = function
  | Literal lit ->
    translate_literal lit  (* Chargement de la valeur *)
    @@ push t0             (* Stockage sur la pile    *)
    
  | Location loc ->
    translate_location loc (* Chargement de l'adresse *)
    @@ lw t0 0 t0          (* Lecture de la valeur    *)
    @@ push t0             (* Stockage sur la pile    *)
    
  | UnaryOp(uop, e) ->
    let op = match uop with (* Sélection de l'opérateur *)
      | Minus -> neg
      | Not -> not_
    in
    translate_expression e (* Calcul de la valeur de e, placée sur la pile *)
    @@ pop t0              (* Récupération de la valeur sur la pile        *)
    @@ op t0 t0            (* Opération            *)
    @@ push t0             (* Stockage sur la pile *)
      
  | BinaryOp(bop, e1, e2) ->
    let op = match bop with (* Sélection de l'opérateur *)
      | Add  -> add
      | Sub  -> sub
      | Mult -> mul
      | Div  -> div
      | Mod  -> rem
      | Eq   -> seq
      | Neq  -> sne
      | Lt   -> slt
      | Le   -> sle
      | Gt   -> sgt
      | Ge   -> sge
      | And  -> and_
      | Or   -> or_
    in
    translate_expression e2    (* Calcul des valeurs des opérandes     *)
    @@ translate_expression e1
    @@ pop t0                  (* Récupération des valeurs sur la pile *)
    @@ pop t1
    @@ op t0 t0 t1             (* Opération            *)
    @@ push t0                 (* Stockage sur la pile *)

      
(* Traduction d'une instruction *)
let rec translate_instruction = function
  | Sequence(i1, i2) ->
    translate_instruction i1    (* Deux instructions en séquence *)
    @@ translate_instruction i2
      
  | Print(e) ->
    translate_expression e  (* Calcul de la valeur du paramètre      *)
    @@ pop a0               (* Récupération de la valeur sur la pile *)
    @@ li v0 11             (* Affichage *)
    @@ syscall
      
  | Set(loc, e) ->
    translate_expression e  (* Calcul de la valeur, stockée sur la pile *)
    @@ translate_location loc  (* Chargement de l'adresse dans t0          *)
    @@ pop t1            (* Récupération de la valeur sur la pile, vers t1 *)
    @@ sw t1 0 t0              (* Écriture                                 *)
      
  | Label(Lab lab) ->  (* Étiquette   *)
    label lab      
      
  | Goto(Lab lab) ->   (* Saut direct *)
    b lab          
      
  | ConditionalGoto(Lab lab, e) ->
    translate_expression e  (* Calcul de la valeur de la garde       *)
    @@ pop t0               (* Récupération de la valeur sur la pile *)
    @@ bnez t0 lab          (* Saut conditionnel                     *)
      
  | Nop -> nop
    
    
let translate_program program =
  (* Initialisation : lit le paramètre donné en entrée et enregistre le résultat
     dans les données statiques sous l'étiquette [arg].
     À défaut de paramètre, [arg] vaudra zéro. *)
  let init =
    beqz a0 "init_end"
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ la t0 "arg"
    @@ sw v0 0 t0
    @@ label "init_end"
      
  (* Terminaison du programme avec l'appel système [exit] *)
  and close =
    li v0 10
    @@ syscall

  (* Fonctions prédéfinies.
     En l'occurrence, fonction de lecture du paramètre d'entrée. *)
  and built_ins =
    (* Le paramètre est donné sous la forme d'une chaîne de caractères
       terminée par le caractère [000]. *)
    label "atoi"
      
    (* Variables *)
    @@ move t0 a0 (* t0 : adresse du caractère à lire *)
    @@ li   t1 0  (* t1 : accumulateur pour la valeur calculée *)
    (* On garde t2 pour des calculs intermédiaires *)
      
    (* Constantes *)
    @@ li   t3 10 (* Base décimale *)
    @@ li   t4 48 (* Code ASCII caractère '0' *)
    @@ li   t5 57 (* Code ASCII caractère '9' *)

    (* Début de la boucle de lecture *)
    @@ label "atoi_loop"
    @@ lbu  t2 0 t0 (* Lecture d'un octet *)

    (* Conditions d'arrêt et d'erreur *)
    @@ beq  t2 zero "atoi_end" (* Fin si lecture de [000] *)
    @@ blt  t2 t4 "atoi_error" (* Erreur si caractère non compris entre 0 et 9 *)
    @@ bgt  t2 t5 "atoi_error"

    (* Mise à jour de l'accumulateur *)
    @@ addi t2 t2 (-48) (* Conversion caractère en nombre *)
    @@ mul  t1 t1 t3
    @@ add  t1 t1 t2 (* t1 <- 10 * t1 + t2 *)

    (* Suite de la lecture *)
    @@ addi t0 t0 1
    @@ b "atoi_loop"

    (* Arrêt du programme en cas d'erreur de lecture *)
    @@ label "atoi_error"
    @@ li   v0 10
    @@ syscall

    (* Renvoi du résultat via [v0] en cas de succès *)
    @@ label "atoi_end"
    @@ move v0 t1
    @@ jr   ra
  in

  (* Construction du texte du programme *)
  let main_code = translate_instruction program.main in
  let text = init @@ main_code @@ close @@ built_ins in

  (* Initialisation de la partie des données statiques *)
  let data = Symb_Tbl.fold
    (fun var _ code -> label var @@ dword [0] @@ code)
    program.globals nop
  in

  (* Programme généré *)
  { text; data }
