%{

  open Lexing
  open CommonAST
  open SourceLocalisedAST
  
%}

%token <int> CONST_INT
%token <bool> CONST_BOOL
%token <string> IDENT
%token PLUS MINUS STAR DIV MOD
%token EQUAL NEQ LE LT GE GT
%token AND OR NOT
%token LP RP

%token VAR
%token INTEGER BOOLEAN

%token MAIN
%token IF ELSE WHILE
%token SEMI
%token SET PRINT
%token BEGIN END
%token EOF

(* Nouveaux lexèmes *)
%token LB RB NEW
%token DOT STRUCT
%token COMMA

%left SEMI
%left AND OR
%left GE GT LE LT EQUAL NEQ
%left PLUS MINUS
%left STAR DIV
%left MOD
%nonassoc NOT
(* Priorités supplémentaires : le symbole [LB] pour l'accès aux tableaux et aux
   structures est plus prioritaire que les autres symboles déjà présents, et
   aussi plus prioritaire que le symbole [NEW]. *)
%nonassoc NEW
%nonassoc LB
%left DOT

%start prog
%type <SourceLocalisedAST.program> prog

%%

prog:
(* Règle mise à jour : un programme est formé d'une séquence de déclarations
   de structure et d'une séquence de déclarations de variables suivies d'un
   bloc de code principal. *)
| structs=struct_decls; globals=var_decls; main=main; EOF
  { let globals = Symb_Tbl.add "arg" TypInt globals
    and functions = Symb_Tbl.empty in
    { main; globals; structs; functions } }

| error { let pos = $startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum pos.pos_cnum
          in
          failwith message }
;

(* Règles pour les déclarations de types de structures.
   Produit une table associant un nom de structure à la liste de ses champs.
   Cette table a le type
     struct_type Symb_Tbl.T
*)
struct_decls:
(* La séquence de déclarations vide donne la table vide. *)    
| (* empty *)                      { Symb_Tbl.empty }
(* Une nouvelle déclaration [sd] est ajoutée à la table [sds]. *)
| sd=struct_decl; sds=struct_decls { let id, s_info = sd in
                                     Symb_Tbl.add id s_info sds }
;

(* Déclaration d'une structure. Produit le nom de cette structure et la liste
   de ses champs. Le résultat a donc le type
     string * struct_type
*)
struct_decl:
| STRUCT; id=IDENT; BEGIN; f=fields_decl; END { id, { fields = f } }
;

(* Liste de déclaration de champs typés. Résultat de type
     (string * typ) list
*)
fields_decl:
| (* empty *)                          { []       }
| tid=typed_ident; SEMI; f=fields_decl { tid :: f }
;

var_decls:
| (* empty *)                      { Symb_Tbl.empty }
| vd=var_decl; SEMI; vds=var_decls { let id, ty = vd in
                                     Symb_Tbl.add id ty vds }
;

var_decl:
| VAR; tid=typed_ident { tid }
;

typed_ident:
| ty=typ; id=IDENT  { id, ty }
;

typ:
| INTEGER        { TypInt       }
| BOOLEAN        { TypBool      }
| ty=typ; LB; RB { TypArray ty  }
(* Nouvelle forme de type : nom de structure. *)
| id=IDENT       { TypStruct id }
;

main:
| MAIN; i=block { i }
;

block:
| BEGIN; i=localised_instruction; END { i }
;

localised_instruction:
| i=instruction { let l = $startpos.pos_lnum in
                  let c = $startpos.pos_cnum - $startpos.pos_bol in
                  mk_instr i l c }
;

instruction:
| (* empty *)                                         { Nop             }
| i1=localised_instruction; SEMI; i2=localised_instruction
                                                      { Sequence(i1,i2) }
| PRINT; LP; e=localised_expression; RP               { Print(e)        }
| loc=location; SET; e=localised_expression           { Set(loc, e)     }
| IF; LP; e=localised_expression; RP; i1=block; ELSE; i2=block
                                               { Conditional(e, i1, i2) }
| WHILE; LP; e=localised_expression; RP; i=block      { Loop(e, i)      }
;

localised_expression:
| e=expression { let l = $startpos.pos_lnum in
                 let c = $startpos.pos_cnum - $startpos.pos_bol in
                 mk_expr e l c }
;

expression:
| lit=literal                                  { Literal(lit)          }
| loc=location                                 { Location(loc)         }
| LP; e=expression; RP                         { e                     }
| uop=unop; e=localised_expression             { UnaryOp(uop, e)       }
| e1=localised_expression; bop=binop; e2=localised_expression
                                               { BinaryOp(bop, e1, e2) }
(* Règles pour la création d'un nouveau tableau ou d'une nouvelle structure. *)
| NEW; ty=typ; LB; e=localised_expression; RB  { NewArray(e, ty)       }
| NEW; ty=typ { match ty with TypStruct(id) -> NewRecord(id) | _ -> assert false }
| id=IDENT; LP; args=arguments; RP
   {
     FunCall(Id(id),args)
   }
;

arguments:
| (* empty *) { [] }
| e=localised_expression; COMMA; args=arguments
   {
     e::args
   }

literal:
| i=CONST_INT   { Int i  }
| b=CONST_BOOL  { Bool b }
;

location:
| id=IDENT                                   { Identifier (Id id)  }
(* Accès à un tableau ou à une structure. *)
| e1=localised_expression; LB; e2=localised_expression; RB
                                             { ArrayAccess(e1, e2) }
| e=localised_expression; DOT; id=IDENT      { FieldAccess(e, id)  }
;

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
