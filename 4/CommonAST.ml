type identifier = Id  of string
type label      = Lab of string

module Symb_Tbl = Map.Make(String)
    
type typ =
  | TypInt
  | TypBool
  | TypArray of typ
  (* Nouveau type : structure nommée. *)
  | TypStruct of string

(* Type d'une structure : liste de champs nommés et typés. *)
type struct_type = {
  fields: (string * typ) list;
}

type function_signature =
  {
    return: typ;
    formals: (string * typ) list;
  }

type function_info =
  {
    signature: function_signature;
    (* D'autres champs seront ajoutés ici en partie B. *)
  }
      
type type_context = {
  identifier_types: typ Symb_Tbl.t;
  (* La table des structures s'ajoute au contexte de typage. *)
  struct_types: struct_type Symb_Tbl.t;
  function_signatures : function_signature Symb_Tbl.t;
}

type literal =
  | Int  of int
  | Bool of bool

type unaryOp = Minus | Not
    
type binaryOp = Add | Sub | Mult | Div | Mod
                | Eq | Neq | Lt | Le | Gt | Ge
                | And | Or
