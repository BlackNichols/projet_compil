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
%token PLUS MINUS STAR DIV MOD
%token EQUAL NEQ LE LT GE GT
%token AND OR NOT
%token LP RP
%token LB RB

%token VAR
%token INTEGER BOOLEAN
%token NEW

%token MAIN
%token IF ELSE WHILE
%token SEMI
%token SET PRINT
%token BEGIN END
%token EOF

(* Priorités et associativités *)
%left SEMI
%left AND OR
%left GE GT LE LT EQUAL NEQ
%left MOD
%left PLUS MINUS
%left STAR DIV
%nonassoc NOT

(* Définition du symbole initial *)
%start prog
%type <SourceLocalisedAST.program> prog

%%

(* Symbole non-terminal principal [prog] *)
prog:
(* Règles : un programme est formé d'une séquence de déclarations de variables
   suivie du bloc de code principal. *)
| globals=var_decls; main=main; EOF
  (* Les déclarations de variables donnent une table des symboles, à laquelle
     est ajoutée la variable spéciale [arg] (avec le type entier). *)
  { let globals = Symb_Tbl.add "arg" TypInt globals in
    { main; globals } }

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
| (* empty *)                      { Symb_Tbl.empty }
(* Sinon, on ajoute la nouvelle déclaration à la table [vds] correspondant
   à l'ensemble de la queue de la séquence de déclarations. *)
| vd=var_decl; SEMI; vds=var_decls { let id, ty = vd in
                                     Symb_Tbl.add id ty vds }
;

(* On isole les notions de type, de variable annotée par un type, et de
   déclaration d'une variable. *)
var_decl:
| VAR; tid=typed_ident { tid }
;

typed_ident:
| ty=typ; id=IDENT  { id, ty }
;

typ:
| INTEGER        { TypInt }
| BOOLEAN        { TypBool }
| t=typ; LB;RB   { TypArray(t) }
;

(* Bloc de code principal, formé du mot-clé [main] suivi par le bloc
   proprement dit. *)
main:
| MAIN; i=block { i }
;

(* Un bloc est une instruction ou séquence d'instructions entre accolades. *)
block:
| BEGIN; i=localised_instruction; END { i }
;

(* Instruction localisée : on mémorise les numéros de ligne et de colonne du
   début de l'instruction.
   Voir dans la doc la définition de [Lexing.position] pour la signification
   de [pos_lnum], [pos_cnum] et [pos_bol]. *)
localised_instruction:
| i=instruction { let l = $startpos.pos_lnum in
                  let c = $startpos.pos_cnum - $startpos.pos_bol in
                  mk_instr i l c }
;

(* Instructions *)
instruction:
(* Si pas d'instruction, on renvoie l'instruction neutre. *)
| (* empty *)                                         { Nop             }
(* Séquence d'instructions (localisées). *)
| i1=localised_instruction; SEMI; i2=localised_instruction
                                                      { Sequence(i1,i2) }
(* Affichage *)
| PRINT; LP; e=localised_expression; RP               { Print(e)        }
(* Affectation *)
| loc=location; SET; e=localised_expression           { Set(loc, e)     }
(* Branchement *)
| IF; LP; e=localised_expression; RP; i1=block; ELSE; i2=block
                                               { Conditional(e, i1, i2) }
(* Boucle *)
| WHILE; LP; e=localised_expression; RP; i=block      { Loop(e, i)      }
;

(* Expression localisée : même principe que [localised_instruction]. *)
localised_expression:
| e=expression { let l = $startpos.pos_lnum in
                 let c = $startpos.pos_cnum - $startpos.pos_bol in
                 mk_expr e l c }
;

(* Expressions. *)
expression:
(* On isole les notions de litéral et d'emplacement mémoire. *)
| lit=literal                                  { Literal(lit)                }
| loc=location                                 { Location(loc)               }
(* Expression entre parenthèses : pour la cohérence des types, on utilise
   [expression] et non [localised_expression] ici. *)
| LP; e=expression; RP                         { e                           }
(* Opérations unaires ou binaires. *)
| uop=unop; e=localised_expression             { UnaryOp(uop, e)             }
| e1=localised_expression; bop=binop; e2=localised_expression
   { BinaryOp(bop, e1, e2) }
| NEW; t=typ; LB; e=localized_expression; RB
  {
    NewArray(t,e)
  }
;

literal:
| i=CONST_INT   { Int i  }
| b=CONST_BOOL  { Bool b }
;

location:
| id=IDENT      { Identifier (Id id) }
| l=location; LB; e = localized_expression; RB
  {
    ArrayAccess(l,e)
  }
;

(* Les opérateurs unaires et binaires sont regroupés sous les symboles
   non-terminaux [unop] et [binop]. *)
%inline unop:
| MINUS { Minus }
| NOT   { Not   }
;

%inline binop:
| PLUS   { Add   }
| MINUS  { Sub   }
| STAR   { Mult  }
| DIV    { Div   }
| MOD    { Mod   }
| EQUAL  { Eq    }
| NEQ    { Neq   }
| LT     { Lt    }
| LE     { Le    }
| GT     { Gt    }
| GE     { Ge    }
| AND    { And   }
| OR     { Or    }
;
(* Avec le mot-clé [%inline], ces règles produisent le même effet que si nous
   avions complètement développé les règles pour les expressions sous la
   forme suivante :
 
   | MINUS; e=localised_expression             { UnaryOp(Minus, e)       }
   | NOT; e=localised_expression               { UnaryOp(Not, e)         }
   | e1=localised_expression; PLUS; e2=localised_expression
                                               { BinaryOp(Plus, e1, e2)  }
   | e1=localised_expression; MINUS; e2=localised_expression
                                               { BinaryOp(Minus, e1, e2) }
   | e1=localised_expression; STAR; e2=localised_expression
                                               { BinaryOp(Star, e1, e2)  }
   | e1=localised_expression; DIV; e2=localised_expression
                                               { BinaryOp(Div, e1, e2)   }
   ...

   En particulier, cela permet aux priorités des différents opérateurs de
   s'exprimer.
*)
