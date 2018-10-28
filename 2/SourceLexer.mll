{

  (* Contexte *)
  open Lexing
  open SourceParser

  (* Traitement des chaînes de caractères alphabétiques *)
  let id_or_keyword =
    (* Définition d'une table des mots-clés et des lexèmes associés *)
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [	"main", MAIN;
	"print", PRINT;
	"while", WHILE;
	"if", IF;
	"else", ELSE;
	"true", CONST_BOOL(true);
	"false", CONST_BOOL(false);
	"var", VAR;
	"integer", INTEGER;
	"boolean", BOOLEAN;
      ] ;
    fun s ->
      (* On cherche la chaîne [s] dans la table. Si on trouve un mot-clé alors
         on le renvoie. *)
      try  Hashtbl.find h s
      (* Et sinon on considère qu'il s'agit d'un identifiant. *)
      with Not_found -> IDENT(s)
}

(* Raccourci : caractères alphabétiques *)
let alpha = ['a'-'z' 'A'-'Z']

(* Expressions régulières définissant les lexèmes *)
rule token = parse
  (* Les espaces, tabulations et retour chariot sont ignorés *)
  | [' ' '\t']
      { token lexbuf }
  | "\n"
      {
	Lexing.new_line lexbuf;
	token lexbuf
      }
  (* Les chaînes alphabétiques sont traitées par la fonction [id_or_keyword]
     pour être associées à des mots-clés ou des identifiants. *)
  | alpha+
      { id_or_keyword (lexeme lexbuf) }
  (* Traitement des nombres *)
  | ((['1'-'9']['0'-'9']*) | "0")
      {
        CONST_INT(int_of_string (Lexing.lexeme lexbuf))
      }
  (* Opérateurs *)
  | "+"
      { PLUS }
  | "-"
      { MINUS }
  | "*"
      { STAR }
  | "/"
      { DIV }
  | "%"
      { MOD }
  | "=="
      { EQUAL }
  | "!="
      { NEQ }
  | "<="
      { LE }
  | "<"
      { LT }
  | ">="
      { GE }
  | ">"
      { GT }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "!"
      { NOT }
  (* Parenthèses *)
  | "("
      { LP }
  | ")"
      { RP }
  (* Début et fin de bloc *)
  | "{"
      { BEGIN }
  | "}"
      { END }
  (* Séquence *)
  | ";"
      { SEMI }
  (* Affectation *)
  | ":="
      { SET }
  | "//"
      { comment lexbuf }
  (* Fin de fichier *)
  | eof
      { EOF }
  (* Caractères non reconnus *)
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }

and comment = parse
    | "\n"
	{ token lexbuf }
    | _
	{ comment lexbuf }
    | eof
	{ EOF }
