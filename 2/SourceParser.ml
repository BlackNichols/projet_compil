
exception Error

let _eRR =
  Error

type token = 
  | WHILE
  | VAR
  | STAR
  | SET
  | SEMI
  | RP
  | PRINT
  | PLUS
  | OR
  | NOT
  | NEQ
  | MOD
  | MINUS
  | MAIN
  | LT
  | LP
  | LE
  | INTEGER
  | IF
  | IDENT of (
# 13 "SourceParser.mly"
       (string)
# 31 "SourceParser.ml"
)
  | GT
  | GE
  | EQUAL
  | EOF
  | END
  | ELSE
  | DIV
  | CONST_INT of (
# 11 "SourceParser.mly"
       (int)
# 43 "SourceParser.ml"
)
  | CONST_BOOL of (
# 12 "SourceParser.mly"
       (bool)
# 48 "SourceParser.ml"
)
  | BOOLEAN
  | BEGIN
  | AND

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state

# 1 "SourceParser.mly"
  

  (* Contexte *)
  open Lexing
  open CommonAST
  open SourceLocalisedAST
  

# 72 "SourceParser.ml"

let rec _menhir_error0 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv57) = Obj.magic _menhir_stack in
    let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv55) = Obj.magic _menhir_stack in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _v : (
# 31 "SourceParser.mly"
      (SourceLocalisedAST.program)
# 86 "SourceParser.ml"
    ) = let _startpos = _startpos__1_ in
    
# 47 "SourceParser.mly"
        ( let pos = _startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum pos.pos_cnum
          in
          failwith message )
# 95 "SourceParser.ml"
     in
    _menhir_goto_prog _menhir_env _menhir_stack _v) : 'freshtv56)) : 'freshtv58)

and _menhir_goto_prog : _menhir_env -> 'ttv_tail -> (
# 31 "SourceParser.mly"
      (SourceLocalisedAST.program)
# 102 "SourceParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv53) = Obj.magic _menhir_stack in
    let (_v : (
# 31 "SourceParser.mly"
      (SourceLocalisedAST.program)
# 110 "SourceParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv51) = Obj.magic _menhir_stack in
    let (_1 : (
# 31 "SourceParser.mly"
      (SourceLocalisedAST.program)
# 117 "SourceParser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv52)) : 'freshtv54)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 31 "SourceParser.mly"
      (SourceLocalisedAST.program)
# 136 "SourceParser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv49) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MAIN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47) = Obj.magic _menhir_stack in
        ((let _v : 'tv_var_decls = 
# 57 "SourceParser.mly"
               ( Symb_Tbl.empty )
# 161 "SourceParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45) = _menhir_stack in
        let (_v : 'tv_var_decls) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * 'tv_var_decls) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MAIN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv39) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv35) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv33) = Obj.magic _menhir_stack in
                ((let (_, _startpos) = Obj.magic _menhir_stack in
                let _v : 'tv_instruction = 
# 85 "SourceParser.mly"
               ( Nop )
# 190 "SourceParser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv31) = _menhir_stack in
                let (_v : 'tv_instruction) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv29) = Obj.magic _menhir_stack in
                let (_v : 'tv_instruction) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv27) = Obj.magic _menhir_stack in
                let (i : 'tv_instruction) = _v in
                let (_startpos_i_ : Lexing.position) = _startpos in
                ((let _v : 'tv_localised_instruction = let _startpos = _startpos_i_ in
                
# 77 "SourceParser.mly"
                ( let l = _startpos.pos_lnum in
                  let c = _startpos.pos_cnum - _startpos.pos_bol in
                  mk_instr i l c )
# 210 "SourceParser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv25) = _menhir_stack in
                let (_v : 'tv_localised_instruction) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv23 * Lexing.position) * 'tv_localised_instruction) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | END ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv19 * Lexing.position) * 'tv_localised_instruction) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv17 * Lexing.position) * 'tv_localised_instruction) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _endpos__1_), i) = _menhir_stack in
                    let _3 = () in
                    let _1 = () in
                    let _v : 'tv_block = 
# 69 "SourceParser.mly"
                                      ( i )
# 233 "SourceParser.ml"
                     in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv15) = _menhir_stack in
                    let (_v : 'tv_block) = _v in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
                    let (_v : 'tv_block) = _v in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
                    let (i : 'tv_block) = _v in
                    ((let _1 = () in
                    let _v : 'tv_main = 
# 64 "SourceParser.mly"
                ( i )
# 248 "SourceParser.ml"
                     in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv9) = _menhir_stack in
                    let (_v : 'tv_main) = _v in
                    ((let _menhir_stack = (_menhir_stack, _v) in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv7 * 'tv_var_decls) * 'tv_main) = Obj.magic _menhir_stack in
                    ((assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | EOF ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : ('freshtv3 * 'tv_var_decls) * 'tv_main) = Obj.magic _menhir_stack in
                        ((let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : ('freshtv1 * 'tv_var_decls) * 'tv_main) = Obj.magic _menhir_stack in
                        ((let ((_menhir_stack, vars), main) = _menhir_stack in
                        let _3 = () in
                        let _v : (
# 31 "SourceParser.mly"
      (SourceLocalisedAST.program)
# 269 "SourceParser.ml"
                        ) = 
# 42 "SourceParser.mly"
  ( { main = main;
      globals = Symb_Tbl.add "arg" TypInt vars; } )
# 274 "SourceParser.ml"
                         in
                        _menhir_goto_prog _menhir_env _menhir_stack _v) : 'freshtv2)) : 'freshtv4)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : ('freshtv5 * 'tv_var_decls) * 'tv_main) = Obj.magic _menhir_stack in
                        ((let ((_menhir_stack, _), _) = _menhir_stack in
                        _menhir_error0 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv6)) : 'freshtv8)) : 'freshtv10)) : 'freshtv12)) : 'freshtv14)) : 'freshtv16)) : 'freshtv18)) : 'freshtv20)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv21 * Lexing.position) * 'tv_localised_instruction) = Obj.magic _menhir_stack in
                    (raise _eRR : 'freshtv22)) : 'freshtv24)) : 'freshtv26)) : 'freshtv28)) : 'freshtv30)) : 'freshtv32)) : 'freshtv34)) : 'freshtv36)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv37) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv38)) : 'freshtv40)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv41 * 'tv_var_decls) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _) = _menhir_stack in
            _menhir_error0 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv42)) : 'freshtv44)) : 'freshtv46)) : 'freshtv48)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_error0 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv50))

# 220 "/usr/share/menhir/standard.mly"
  


# 312 "SourceParser.ml"
