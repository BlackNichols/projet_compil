open CommonAST
open GotoAST
open Mips

(* Fonctions auxiliaires fournissant les pseudo-instructions [push] et [pop]. *)
let push reg = sw reg 0 sp  @@ subi sp sp 4
let pop  reg = addi sp sp 4 @@ lw reg 0 sp

(**
   Fonction de traduction des expressions.
   [translate_expression : GotoAST.expression -> Mips.text]

   Rappel de la convention : le code généré par [translate_expression e] doit
   placer la valeur de l'expression [e] au sommet de la pile.
*)
let translate_literal = function
  | Int(i) ->
     li t0 i
     @@ push t0
  | Bool(true) ->
     li t0 (-1)
     @@ push t0
  | Bool(false) ->
     li t0 0
     @@ push t0
     
let rec translate_expression (e: GotoAST.expression) = match e with
  | Literal(l) ->
     translate_literal l
  | Location(Identifier(Id(l))) ->
     la t0 l
     @@ push t0
  | UnaryOp(u,e) ->
     translate_uop u e
  | BinaryOp(u,e1,e2) ->
     translate_bop u e1 e2

and translate_uop u e =
  let aux i =
    translate_expression e
    @@ pop t0
    @@ i
    @@ push t0
  in
  match u with
  | Minus ->
     aux (neg t0 t0)
  | Not ->
     aux (not_ t0 t0)

and translate_bop b e1 e2 =
  let aux i =
    translate_expression e1
     @@ pop t0
     @@ translate_expression e2
     @@ pop t1
     @@ i
     @@ push t0
  in
  match b with
  | Add ->
     aux (add t0 t0 t1)
  | Sub ->
     aux (sub t0 t0 t1)
  | Mult ->
     aux (mul t0 t0 t1) 
  | Div ->
     aux (div t0 t0 t1)
  | Mod ->
     aux (rem t0 t0 t1)
  | Eq ->
     aux ((seq t0 t0 t1)@@(neg t0 t0))
  | Neq ->
     aux ((sne t0 t0 t1)@@(neg t0 t0))
  | Lt ->
     aux ((slt t0 t0 t1)@@(neg t0 t0))
  | Le ->
     aux ((sle t0 t0 t1)@@(neg t0 t0))
  | Gt ->
     aux ((sgt t0 t0 t1)@@(neg t0 t0))
  | Ge ->
     aux ((sge t0 t0 t1)@@(neg t0 t0))
  | And ->
     aux ((and_ t0 t0 t1)@@(neg t0 t0))
  | Or ->
     aux ((or_ t0 t0 t1)@@(neg t0 t0))
     
(**
   Fonction de traduction des instructions.
   [translate_instruction : GotoAST.instruction -> Mips.text]
*)
let rec translate_instruction (i: GotoAST.instruction) = match i with
  | Sequence(i1,i2) ->
     (translate_instruction i1)
     @@ (translate_instruction i2)
  | Print(e) ->
     translate_expression e
     @@ pop a0
     @@ li v0 1
     @@ syscall
  | Set(Identifier(Id(s)),e) ->
     la t0 s
     @@ translate_expression e
     @@ pop t1
     @@ sw t1 0 t0
  | Label(Lab(l)) ->
     label l
  | Goto(Lab(l)) ->
     b l
  | ConditionalGoto(Lab(l),e) ->
     translate_expression e
     @@ pop t0
     @@ bnez t0 l
  | Nop -> nop


(** 
    Fonction de traduction des programmes
    [translate_program : GotoAST.program -> Mips.program]

    Rien à changer dans cette fonction, elle fournit déjà l'infrastructure dans
    laquelle insérer le code principal.
*)
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
