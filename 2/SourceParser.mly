%{

  (* Contexte *)
  open Lexing
  open CommonAST
  open SourceLocalisedAST
  
%}

(* Définition des lexèmes *)
%token <int> CONST_INT
%token <bool> CONST_BOOL
%token <string> IDENT
%token PLUS MINUS
%token STAR DIV MOD
%token EQUAL NEQ LE LT GE GT
%token AND OR NOT
%token LP RP
%token LB RB

%token VAR NEW
%token INTEGER BOOLEAN

%token MAIN
%token IF ELSE WHILE
%token SEMI
%token SET PRINT
%token BEGIN END
%token EOF

%left OR
%left AND
%left EQUAL NEQ LE LT GE GT
%left PLUS MINUS
%left STAR DIV MOD
%right NOT UMINUS
%left SEMI


(* Définition du symbole initial *)
%start prog
%type <SourceLocalisedAST.program> prog
%type <typ Symb_Tbl.t> var_decls
%type <SourceLocalisedAST.instruction> instruction

%%

(* Symbole non-terminal principal [prog] *)
prog:
(* Règles : un programme est formé d'une séquence de déclarations de variables
   suivie du bloc de code principal. *)
| vars=var_decls; main=main; EOF
  (* Les déclarations de variables donnent une table des symboles, à laquelle
     est ajoutée la variable spéciale [arg] (avec le type entier). *)
  { { main = main;
      globals = Symb_Tbl.add "arg" TypInt vars; } }
  
(* Aide : ajout d'une règle pour récupérer grossièrement les erreurs se 
   propageant jusqu'à la racine. *)
| error { let pos = $startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum pos.pos_cnum
          in
          failwith message }
;

(* Séquence de déclaration de variables *)
  var_decls:
(* Si pas de déclaration, on renvoie la table vide. *)
| (* empty *)  { Symb_Tbl.empty }
(* Sinon : à compléter ! *)
| VAR; INTEGER; id = IDENT; SEMI; vars = var_decls
   {
     Symb_Tbl.add id TypInt vars
   }
| VAR; BOOLEAN; id = IDENT; SEMI; vars = var_decls
   {
     Symb_Tbl.add id TypBool vars
   }
;

(* Bloc de code principal, formé du mot-clé [main] suivi par le bloc
   proprement dit. *)
  main:
| MAIN; i=block
   {
     i
   }
;

(* Un bloc est une instruction ou séquence d'instructions entre accolades. *)
block:
| BEGIN; i=localised_instruction; END
  {
    i
  }
;

(* Instruction localisée : on mémorise les numéros de ligne et de colonne du
   début de l'instruction.
   Voir dans la doc la définition de [Lexing.position] pour la signification
   de [pos_lnum], [pos_cnum] et [pos_bol]. *)
localised_instruction:
| i=instruction
   {
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     mk_instr i l c
   }
;

(* Instructions *)
instruction:
(* Si pas d'instruction, on renvoie l'instruction neutre. *)
| (* empty *)  { Nop }
(* Sinon : à compléter ! *)
| PRINT; exp = localised_expression
   {
     Print(exp)
   }
| l = location; SET; exp = localised_expression
   {
     Set(l,exp)
   }
| IF; LP; exp = localised_expression; RP; i1 = block; ELSE; i2 = block
   {
     Conditional(exp,i1,i2)
   }
| WHILE; LP; exp = localised_expression; RP; i = block
   {
     Loop(exp,i)
   }
| i1 = localised_instruction; SEMI; i2 = localised_instruction
   {
     Sequence(i1,i2)
   }
;

localised_expression:
| e=expression
   {
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     mk_expr e l c
   }
;

expression:
| n = CONST_INT
   {
     Literal(Int(n))
   }
| b = CONST_BOOL
   {
     Literal(Bool(b))
   }
| l = location
   {
     l
   }
| LP; e = expression; RP
  {
    e
  }
| MINUS; e = localised_expression %prec UMINUS
   {
     UnaryOp(Minus, e)
   }
| NOT; e = localised_expression
   {
     UnaryOp(Not, e)
   }
| e1 = localised_expression; PLUS; e2 = localised_expression
  {
    BinaryOp(Add,e1,e2)
  }
| e1 = localised_expression; MINUS; e2 = localised_expression
  {
    BinaryOp(Sub,e1,e2)
  }
| e1 = localised_expression; STAR; e2 = localised_expression
  {
    BinaryOp(Mult,e1,e2)
  }
| e1 = localised_expression; DIV; e2 = localised_expression
  {
    BinaryOp(Div,e1,e2)
  }
| e1 = localised_expression; MOD; e2 = localised_expression
  {
    BinaryOp(Mod,e1,e2)
  }
| e1 = localised_expression; EQUAL; e2 = localised_expression
  {
    BinaryOp(Eq,e1,e2)
  }
| e1 = localised_expression; NEQ; e2 = localised_expression
  {
    BinaryOp(Neq,e1,e2)
  }
| e1 = localised_expression; GE; e2 = localised_expression
  {
    BinaryOp(Ge,e1,e2)
  }
| e1 = localised_expression; GT; e2 = localised_expression
  {
    BinaryOp(Gt,e1,e2)
  }
| e1 = localised_expression; LE; e2 = localised_expression
  {
    BinaryOp(Le,e1,e2)
  }
| e1 = localised_expression; LT; e2 = localised_expression
  {
    BinaryOp(Lt,e1,e2)
  }
| e1 = localised_expression; AND; e2 = localised_expression
  {
    BinaryOp(And,e1,e2)
  }
| e1 = localised_expression; OR; e2 = localised_expression
  {
    BinaryOp(Or,e1,e2)
  }
;

location :
| id = IDENT
   {
     Location(Identifier(Id(id)))
   }