{

  (* Contexte *)
  open Lexing
  open SourceParser

  (* Traitement des chaînes de caractères alphabétiques *)
  let id_or_keyword =
    (* Définition d'une table des mots-clés et des lexèmes associés *)
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "true",     CONST_BOOL(true);
	"false",    CONST_BOOL(false);
	"while",    WHILE;
	"if",       IF;
	"else",     ELSE;
	"print",    PRINT;
	"main",     MAIN;
	"var",      VAR;
        "integer",  INTEGER;
        "boolean",  BOOLEAN;
	"new",      NEW;
      ] ;
    fun s ->
      (* On cherche la chaîne [s] dans la table. Si on trouve un mot-clé alors
         on le renvoie. *)
      try  Hashtbl.find h s
      (* Et sinon on considère qu'il s'agit d'un identifiant. *)
      with Not_found -> IDENT(s)
        
}

(* Raccourcis : chiffres, caractères alphabétiques, identifiants *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | '\'' | digit)*
  
(* Expressions régulières définissant les lexèmes.
   Définit une fonction [token : Lexing.lexbuf -> SourceParser.token]. *)
rule token = parse
  (* Les espaces et tabulations sont ignorés *)
  | [' ' '\t']+
      { token lexbuf }
  (* Les retours à la ligne sont traités comme les espaces et tabulations,
     avec en plus une mise à jour du compteur de lignes. *)
  | ['\n']
      { new_line lexbuf; token lexbuf }
  (* Les commentaires sont également ignorés, mais leur reconnaissance est
     déléguée à une fonction spécifique. *)
  | "//"
      { comment lexbuf; token lexbuf }
  (* Les suites de chiffres sont converties en nombres entiers. *)
  | digit+
      { CONST_INT (int_of_string (lexeme lexbuf)) }
  (* Les chaînes alphabétiques sont traitées par la fonction [id_or_keyword]
     pour être associées à des mots-clés ou des identifiants. *)
  | ident
      { id_or_keyword (lexeme lexbuf) }
  (* Début et fin de bloc, et autres symboles simples *)
  | "["
      { LB }
  | "]"
      { RB }
  | "{"
      { BEGIN }
  | "}"
      { END }
  | "("
      { LP }
  | ")"
      { RP }
  | ";"
      { SEMI }
  | ":="
      { SET }
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
  | "<"
      { LT }
  | "<="
      { LE }
  | ">"
      { GT }
  | ">="
      { GE }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "!"
      { NOT }
  (* Caractères non reconnus *)
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  (* Fin de fichier *)
  | eof
      { EOF }

(* Règles dédiée pour reconnaître les commentaires.
   Définit une fonction [comment : Lexing.lexbuf -> unit]. *)
and comment = parse
  (* Tous les caractères autres que le retour à la ligne sont ignorés *)
  | [^ '\n']*
      { comment lexbuf }
  (* On s'arrête si retour à la ligne ou fin d fichier *)
  | '\n'
      { new_line lexbuf }
  | eof
      { () }
    
