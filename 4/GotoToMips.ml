open CommonAST
open GotoAST
open Mips

let push reg = sw reg 0 sp  @@ subi sp sp 4
let pop  reg = addi sp sp 4 @@ lw reg 0 sp

let translate_literal = function
  | Int i  -> li t0 i
  | Bool b -> li t0 (if b then -1 else 0)

(* Chargement de l'adresse d'un emplacement dans le registre $t0 *)
let rec translate_location = function
  | Identifier (Id id) -> la t0 id

  (* Cas de l'accès à un bloc : on ajoute 4 fois l'indice du champ à l'adresse
     de base du bloc. *)
  | BlockAccess (a, i) ->
    translate_expression i (* Calcul de l'indice [i] du champ. *)
    @@ pop t0        (* $t0 <- i      *)
    @@ li t1 4      
    @@ mul t0 t0 t1  (* $t0 <- 4i     *)
    @@ push t0
    @@ translate_expression a (* Calcul de l'adresse de base [a]. *)
    @@ pop t0        (* $t0 <- a      *)
    @@ pop t1        (* $t1 <- 4i     *)
    @@ add t0 t0 t1  (* $t0 <- a + 4i *)


(* Calcul de la valeur d'une expression. La valeur résultat est placée
   au sommet de la pile. *)
and translate_expression = function
  | Literal lit ->
    translate_literal lit
    @@ push t0           
    
  | Location loc ->
    translate_location loc (* Chargement de l'adresse dans t0 *)
    @@ lw t0 0 t0          (* Lecture de la valeur            *)
    @@ push t0             (* Stockage sur la pile            *)
    
  | UnaryOp(uop, e) ->
    let op = match uop with  
      | Minus -> neg
      | Not -> not_
    in
    translate_expression e 
    @@ pop t0              
    @@ op t0 t0            
    @@ push t0             
      
  | BinaryOp(bop, e1, e2) ->
    let op = match bop with
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
    translate_expression e2   
    @@ translate_expression e1
    @@ pop t0                 
    @@ pop t1
    @@ op t0 t0 t1            
    @@ push t0                

  (* Allocation d'un bloc et initialisation de l'en-tête. *)
  | NewBlock(e) ->
    (* Placer dans $a0 le nombre d'octets à allouer : 4*(e+1) *)
    translate_expression e
    @@ pop t0       (* $t0 <- e *)
    @@ push t0      (* Conserver la valeur de e dans la pile *)
    (* Les deux instructions précédentes pourraient être remplacées par
       [peek t0] définie par [lw t0 4 sp] *)
    @@ addi t0 t0 1 (* $t0 <- e+1 *)
    @@ li t1 4
    @@ mul a0 t0 t1 (* $a0 <- (e+1)*4 *)
    (* Appel système sbrk *)
    @@ li v0 9
    @@ syscall
    (* $v0 contient l'adresse du bloc alloué (ie : adresse de l'en-tête) *)
    (* Initialisation de l'en-tête avec le nombre de champs *)
    @@ pop t0
    @@ sw t0 0 v0
    (* Renvoyer l'adresse du premier champ, via la pile *)
    @@ addi t0 v0 4 (* $t0 <- adresse du premier champ *)
    @@ push t0
  | FunCall(id,args) ->
     fold_right (fun e acc -> acc @@ translate_expression e) args ""
     @@ jal id
     @@ pop t0
     @@ fold_left (fun _ _ -> pop t1) args ""
     @@ push t0
      
let rec translate_instruction = function
  | Sequence(i1, i2) ->
    translate_instruction i1   
    @@ translate_instruction i2
      
  | Print(e) ->
    translate_expression e 
    @@ pop a0              
    @@ li v0 11
    @@ syscall
      
  | Set(loc, e) ->
    translate_expression e 
    @@ translate_location loc
    @@ pop t1           
    @@ sw t1 0 t0       
      
  | Label(Lab lab) -> 
    label lab      
      
  | Goto(Lab lab) ->  
    b lab          
      
  | ConditionalGoto(Lab lab, e) ->
    translate_expression e
    @@ pop t0             
    @@ bnez t0 lab        
      
  | Nop -> nop
    
    
let translate_program program =
  let init =
    beqz a0 "init_end"
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ la t0 "arg"
    @@ sw v0 0 t0
    @@ label "init_end"
      
  and close =
    li v0 10
    @@ syscall

  and built_ins =
    label "atoi"      
    @@ move t0 a0 
    @@ li   t1 0  
    @@ li   t3 10 
    @@ li   t4 48 
    @@ li   t5 57 
    @@ label "atoi_loop"
    @@ lbu  t2 0 t0 
    @@ beq  t2 zero "atoi_end" 
    @@ blt  t2 t4 "atoi_error" 
    @@ bgt  t2 t5 "atoi_error"
    @@ addi t2 t2 (-48) 
    @@ mul  t1 t1 t3
    @@ add  t1 t1 t2 
    @@ addi t0 t0 1
    @@ b "atoi_loop"
    @@ label "atoi_error"
    @@ li   v0 10
    @@ syscall
    @@ label "atoi_end"
    @@ move v0 t1
    @@ jr   ra
    @@ comment "print_int"
    @@ label "print_int"
    @@ lw a0 4 sp
    @@ li v0 1
    @@ syscall
    @@ sw a0 0 sp
    @@ subi sp sp 4
    @@ jr ra
      
    @@ comment "power"
    @@ label "power"
    @@ lw s0 8 sp
    @@ lw s1 4 sp
    @@ li t0 1
    @@ b "power_loop_guard"
    @@ label "power_loop_code"
    @@ mul t0 t0 s1
    @@ subi s0 s0 1
    @@ label "power_loop_guard"
    @@ bgtz s0 "power_loop_code"
    @@ sw t0 0 sp
    @@ subi sp sp 4
    @@ jr ra

  in

  let main_code = translate_instruction program.main in
  let text = init @@ main_code @@ close @@ built_ins in

  let data = Symb_Tbl.fold
    (fun var _ code -> label var @@ dword [0] @@ code)
    program.globals nop
  in

  { text; data }
