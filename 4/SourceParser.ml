
exception Error

let _eRR =
  Error

type token = 
  | WHILE
  | VAR
  | STRUCT
  | STAR
  | SET
  | SEMI
  | RP
  | RB
  | PRINT
  | PLUS
  | OR
  | NOT
  | NEW
  | NEQ
  | MOD
  | MINUS
  | MAIN
  | LT
  | LP
  | LE
  | LB
  | INTEGER
  | IF
  | IDENT of (
# 11 "SourceParser.mly"
       (string)
# 35 "SourceParser.ml"
)
  | GT
  | GE
  | EQUAL
  | EOF
  | END
  | ELSE
  | DOT
  | DIV
  | CONST_INT of (
# 9 "SourceParser.mly"
       (int)
# 48 "SourceParser.ml"
)
  | CONST_BOOL of (
# 10 "SourceParser.mly"
       (bool)
# 53 "SourceParser.ml"
)
  | COMMA
  | BOOLEAN
  | BEGIN
  | AND

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState109
  | MenhirState107
  | MenhirState99
  | MenhirState96
  | MenhirState93
  | MenhirState91
  | MenhirState89
  | MenhirState85
  | MenhirState82
  | MenhirState71
  | MenhirState68
  | MenhirState66
  | MenhirState64
  | MenhirState62
  | MenhirState60
  | MenhirState58
  | MenhirState56
  | MenhirState54
  | MenhirState52
  | MenhirState48
  | MenhirState44
  | MenhirState41
  | MenhirState39
  | MenhirState37
  | MenhirState32
  | MenhirState30
  | MenhirState29
  | MenhirState28
  | MenhirState26
  | MenhirState25
  | MenhirState24
  | MenhirState22
  | MenhirState21
  | MenhirState18
  | MenhirState17
  | MenhirState11
  | MenhirState9
  | MenhirState4
  | MenhirState0

# 1 "SourceParser.mly"
  

  open Lexing
  open CommonAST
  open SourceLocalisedAST
  

# 116 "SourceParser.ml"

let rec _menhir_reduce27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _v : 'tv_instruction = 
# 132 "SourceParser.mly"
                                                      ( Nop             )
# 124 "SourceParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv427 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CONST_BOOL _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CONST_INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEW ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24) : 'freshtv428)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv429 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv430)

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv423 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CONST_BOOL _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CONST_INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEW ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv424)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv425 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv426)

and _menhir_run88 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv419 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CONST_BOOL _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CONST_INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEW ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89) : 'freshtv420)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv421 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv422)

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_instruction -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv417) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_instruction) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv415) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (i : 'tv_instruction) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _v : 'tv_localised_instruction = let _startpos = _startpos_i_ in
    
# 126 "SourceParser.mly"
                ( let l = _startpos.pos_lnum in
                  let c = _startpos.pos_cnum - _startpos.pos_bol in
                  mk_instr i l c )
# 267 "SourceParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv413) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_localised_instruction) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv407 * Lexing.position * _menhir_state) * _menhir_state * 'tv_localised_instruction * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv401 * Lexing.position * _menhir_state) * _menhir_state * 'tv_localised_instruction * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv399 * Lexing.position * _menhir_state) * _menhir_state * 'tv_localised_instruction * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s), _, i, _startpos_i_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_block = 
# 122 "SourceParser.mly"
                                      ( i )
# 294 "SourceParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv397) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_block) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState82 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv371 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv369 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, e, _startpos_e_), _, i) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _v : 'tv_instruction = 
# 139 "SourceParser.mly"
                                                      ( Loop(e, i)      )
# 315 "SourceParser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv370)) : 'freshtv372)
            | MenhirState91 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv377 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ELSE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (((('freshtv373 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | BEGIN ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv374)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (((('freshtv375 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv376)) : 'freshtv378)
            | MenhirState93 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv381 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv379 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, e, _startpos_e_), _, i1), _, i2) = _menhir_stack in
                let _6 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _v : 'tv_instruction = 
# 138 "SourceParser.mly"
                                               ( Conditional(e, i1, i2) )
# 357 "SourceParser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv380)) : 'freshtv382)
            | MenhirState21 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv395) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv393) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, i) = _menhir_stack in
                let _1 = () in
                let _v : 'tv_main = 
# 118 "SourceParser.mly"
                ( i )
# 370 "SourceParser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv391) = _menhir_stack in
                let (_v : 'tv_main) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv389 * _menhir_state * 'tv_struct_decls) * _menhir_state * 'tv_var_decls) * 'tv_main) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | EOF ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv385 * _menhir_state * 'tv_struct_decls) * _menhir_state * 'tv_var_decls) * 'tv_main) = Obj.magic _menhir_stack in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv383 * _menhir_state * 'tv_struct_decls) * _menhir_state * 'tv_var_decls) * 'tv_main) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s, structs), _, globals), main) = _menhir_stack in
                    let _4 = () in
                    let _v : (
# 47 "SourceParser.mly"
      (SourceLocalisedAST.program)
# 391 "SourceParser.ml"
                    ) = 
# 56 "SourceParser.mly"
  ( let globals = Symb_Tbl.add "arg" TypInt globals in
    { main; globals; structs } )
# 396 "SourceParser.ml"
                     in
                    _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v) : 'freshtv384)) : 'freshtv386)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv387 * _menhir_state * 'tv_struct_decls) * _menhir_state * 'tv_var_decls) * 'tv_main) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv388)) : 'freshtv390)) : 'freshtv392)) : 'freshtv394)) : 'freshtv396)
            | _ ->
                _menhir_fail ()) : 'freshtv398)) : 'freshtv400)) : 'freshtv402)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv403 * _menhir_state * 'tv_localised_instruction * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONST_INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEW ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PRINT ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHILE ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | END | SEMI ->
                _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv404)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv405 * Lexing.position * _menhir_state) * _menhir_state * 'tv_localised_instruction * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv406)) : 'freshtv408)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv411 * _menhir_state * 'tv_localised_instruction * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_instruction * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv409 * _menhir_state * 'tv_localised_instruction * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_instruction * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, i1, _startpos_i1_), _endpos__2_), _, i2, _startpos_i2_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_i1_ in
        let _v : 'tv_instruction = 
# 134 "SourceParser.mly"
                                                      ( Sequence(i1,i2) )
# 460 "SourceParser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv410)) : 'freshtv412)
    | _ ->
        _menhir_fail ()) : 'freshtv414)) : 'freshtv416)) : 'freshtv418)

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv365 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "SourceParser.mly"
       (string)
# 766 "SourceParser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv363 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        let (id : (
# 11 "SourceParser.mly"
       (string)
# 775 "SourceParser.ml"
        )) = _v in
        let (_startpos_id_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _menhir_s, e, _startpos_e_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_e_ in
        let _v : 'tv_location = 
# 181 "SourceParser.mly"
                                             ( FieldAccess(e, id)  )
# 784 "SourceParser.ml"
         in
        _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv364)) : 'freshtv366)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv367 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_localised_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_reduce5 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_location * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, loc, _startpos_loc_) = _menhir_stack in
    let _startpos = _startpos_loc_ in
    let _v : 'tv_expression = 
# 150 "SourceParser.mly"
                                               ( Location(loc)         )
# 850 "SourceParser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_goto_arguments : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_arguments -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv353 * _menhir_state * 'tv_expression * Lexing.position)) * _menhir_state * 'tv_arguments) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv351 * _menhir_state * 'tv_expression * Lexing.position)) * _menhir_state * 'tv_arguments) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, e, _startpos_e_), _, args) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_arguments = 
# 167 "SourceParser.mly"
   (
     e::args
   )
# 870 "SourceParser.ml"
         in
        _menhir_goto_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv352)) : 'freshtv354)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv361 * _menhir_state * (
# 11 "SourceParser.mly"
       (string)
# 878 "SourceParser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_arguments) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv357 * _menhir_state * (
# 11 "SourceParser.mly"
       (string)
# 888 "SourceParser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_arguments) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv355 * _menhir_state * (
# 11 "SourceParser.mly"
       (string)
# 895 "SourceParser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_arguments) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, id, _startpos_id_), _startpos__2_), _, args) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_id_ in
            let _v : 'tv_expression = 
# 159 "SourceParser.mly"
   (
     FunCall(id,args)
   )
# 906 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv356)) : 'freshtv358)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv359 * _menhir_state * (
# 11 "SourceParser.mly"
       (string)
# 916 "SourceParser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_arguments) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv360)) : 'freshtv362)
    | _ ->
        _menhir_fail ()

and _menhir_run22 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PRINT ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | END | SEMI ->
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_reduce35 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, e, _startpos_e_) = _menhir_stack in
    let _startpos = _startpos_e_ in
    let _v : 'tv_localised_expression = let _startpos = _startpos_e_ in
    
# 143 "SourceParser.mly"
               ( let l = _startpos.pos_lnum in
                 let c = _startpos.pos_cnum - _startpos.pos_bol in
                 mk_expr e l c )
# 966 "SourceParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv349) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_localised_expression) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState22 | MenhirState99 | MenhirState30 | MenhirState71 | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv215 * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv213 * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)) : 'freshtv216)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv221 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | END | EQUAL | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RB | RP | SEMI | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv217 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _v : 'tv_expression = let bop =
              let _1 = _10 in
              
# 192 "SourceParser.mly"
         ( Mult  )
# 1041 "SourceParser.ml"
              
            in
            
# 154 "SourceParser.mly"
                                               ( BinaryOp(bop, e1, e2) )
# 1047 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv218)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv219 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)) : 'freshtv222)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv227 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | END | EQUAL | GE | GT | LE | LT | MINUS | MOD | NEQ | OR | PLUS | RB | RP | SEMI | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv223 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _v : 'tv_expression = let bop =
              let _1 = _10 in
              
# 194 "SourceParser.mly"
         ( Mod   )
# 1078 "SourceParser.ml"
              
            in
            
# 154 "SourceParser.mly"
                                               ( BinaryOp(bop, e1, e2) )
# 1084 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv224)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv225 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)) : 'freshtv228)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv235 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv231 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv229 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : 'tv_location = 
# 180 "SourceParser.mly"
                                             ( ArrayAccess(e1, e2) )
# 1141 "SourceParser.ml"
             in
            _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv230)) : 'freshtv232)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv233 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)) : 'freshtv236)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv241 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQUAL | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RB | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv237 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _v : 'tv_expression = let bop =
              let _1 = _10 in
              
# 190 "SourceParser.mly"
         ( Add   )
# 1180 "SourceParser.ml"
              
            in
            
# 154 "SourceParser.mly"
                                               ( BinaryOp(bop, e1, e2) )
# 1186 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv238)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv239 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)) : 'freshtv242)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv247 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | END | EQUAL | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RB | RP | SEMI | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv243 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _v : 'tv_expression = let bop =
              let _1 = _10 in
              
# 193 "SourceParser.mly"
         ( Div   )
# 1219 "SourceParser.ml"
              
            in
            
# 154 "SourceParser.mly"
                                               ( BinaryOp(bop, e1, e2) )
# 1225 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv244)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv245 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)) : 'freshtv248)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv253 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | OR | RB | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv249 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _v : 'tv_expression = let bop =
              let _1 = _10 in
              
# 202 "SourceParser.mly"
         ( Or    )
# 1278 "SourceParser.ml"
              
            in
            
# 154 "SourceParser.mly"
                                               ( BinaryOp(bop, e1, e2) )
# 1284 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv250)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv251 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)) : 'freshtv254)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv259 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQUAL | GE | GT | LE | LT | NEQ | OR | RB | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv255 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _v : 'tv_expression = let bop =
              let _1 = _10 in
              
# 196 "SourceParser.mly"
         ( Neq   )
# 1325 "SourceParser.ml"
              
            in
            
# 154 "SourceParser.mly"
                                               ( BinaryOp(bop, e1, e2) )
# 1331 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv256)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv257 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)) : 'freshtv260)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv265 * _menhir_state * 'tv_localised_expression * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQUAL | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RB | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv261 * _menhir_state * 'tv_localised_expression * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, e1, _startpos_e1_), _startpos__10_), _, e2, _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _v : 'tv_expression = let bop =
              let _1 = _10 in
              
# 191 "SourceParser.mly"
         ( Sub   )
# 1368 "SourceParser.ml"
              
            in
            
# 154 "SourceParser.mly"
                                               ( BinaryOp(bop, e1, e2) )
# 1374 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv262)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv263 * _menhir_state * 'tv_localised_expression * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)) : 'freshtv266)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv271 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQUAL | GE | GT | LE | LT | NEQ | OR | RB | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv267 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _v : 'tv_expression = let bop =
              let _1 = _10 in
              
# 197 "SourceParser.mly"
         ( Lt    )
# 1415 "SourceParser.ml"
              
            in
            
# 154 "SourceParser.mly"
                                               ( BinaryOp(bop, e1, e2) )
# 1421 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv268)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv269 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv270)) : 'freshtv272)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv277 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQUAL | GE | GT | LE | LT | NEQ | OR | RB | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv273 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _v : 'tv_expression = let bop =
              let _1 = _10 in
              
# 198 "SourceParser.mly"
         ( Le    )
# 1462 "SourceParser.ml"
              
            in
            
# 154 "SourceParser.mly"
                                               ( BinaryOp(bop, e1, e2) )
# 1468 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv274)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv275 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)) : 'freshtv278)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv283 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQUAL | GE | GT | LE | LT | NEQ | OR | RB | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv279 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _v : 'tv_expression = let bop =
              let _1 = _10 in
              
# 199 "SourceParser.mly"
         ( Gt    )
# 1509 "SourceParser.ml"
              
            in
            
# 154 "SourceParser.mly"
                                               ( BinaryOp(bop, e1, e2) )
# 1515 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv280)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv281 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)) : 'freshtv284)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv289 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQUAL | GE | GT | LE | LT | NEQ | OR | RB | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv285 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _v : 'tv_expression = let bop =
              let _1 = _10 in
              
# 200 "SourceParser.mly"
         ( Ge    )
# 1556 "SourceParser.ml"
              
            in
            
# 154 "SourceParser.mly"
                                               ( BinaryOp(bop, e1, e2) )
# 1562 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv286)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv287 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)) : 'freshtv290)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv295 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQUAL | GE | GT | LE | LT | NEQ | OR | RB | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv291 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _v : 'tv_expression = let bop =
              let _1 = _10 in
              
# 195 "SourceParser.mly"
         ( Eq    )
# 1603 "SourceParser.ml"
              
            in
            
# 154 "SourceParser.mly"
                                               ( BinaryOp(bop, e1, e2) )
# 1609 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv292)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv293 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv294)) : 'freshtv296)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv301 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | OR | RB | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv297 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _v : 'tv_expression = let bop =
              let _1 = _10 in
              
# 201 "SourceParser.mly"
         ( And   )
# 1662 "SourceParser.ml"
              
            in
            
# 154 "SourceParser.mly"
                                               ( BinaryOp(bop, e1, e2) )
# 1668 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv298)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv299 * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)) : 'freshtv302)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv307 * _menhir_state * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQUAL | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RB | RP | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv303 * _menhir_state * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__10_), _, e, _startpos_e_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _v : 'tv_expression = let uop =
              let _1 = _10 in
              
# 185 "SourceParser.mly"
        ( Minus )
# 1705 "SourceParser.ml"
              
            in
            
# 152 "SourceParser.mly"
                                               ( UnaryOp(uop, e)       )
# 1711 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv304)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv305 * _menhir_state * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)) : 'freshtv308)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv315 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | RB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv311 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv309 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, ty), _, e, _startpos_e_) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : 'tv_expression = 
# 156 "SourceParser.mly"
                                               ( NewArray(e, ty)       )
# 1769 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv310)) : 'freshtv312)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv313 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv314)) : 'freshtv316)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv321 * _menhir_state * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | END | EQUAL | GE | GT | LE | LT | MINUS | MOD | NEQ | OR | PLUS | RB | RP | SEMI | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv317 * _menhir_state * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__10_), _, e, _startpos_e_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _v : 'tv_expression = let uop =
              let _1 = _10 in
              
# 186 "SourceParser.mly"
        ( Not   )
# 1802 "SourceParser.ml"
              
            in
            
# 152 "SourceParser.mly"
                                               ( UnaryOp(uop, e)       )
# 1808 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv318)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv319 * _menhir_state * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv327 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv323 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState82
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv324)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv325 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv326)) : 'freshtv328)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv335 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv331 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv329 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, e, _startpos_e_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : 'tv_instruction = 
# 135 "SourceParser.mly"
                                                      ( Print(e)        )
# 1921 "SourceParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv330)) : 'freshtv332)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv333 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv334)) : 'freshtv336)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv341 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv337 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91) : 'freshtv338)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv339 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)) : 'freshtv342)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv347 * _menhir_state * 'tv_location * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | END | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv343 * _menhir_state * 'tv_location * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, loc, _startpos_loc_), _, e, _startpos_e_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_loc_ in
            let _v : 'tv_instruction = 
# 136 "SourceParser.mly"
                                                      ( Set(loc, e)     )
# 2033 "SourceParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv344)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv345 * _menhir_state * 'tv_location * Lexing.position)) * _menhir_state * 'tv_localised_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv346)) : 'freshtv348)
    | _ ->
        _menhir_fail ()) : 'freshtv350)

and _menhir_goto_location : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_location -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState96 | MenhirState89 | MenhirState85 | MenhirState24 | MenhirState25 | MenhirState28 | MenhirState29 | MenhirState30 | MenhirState71 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState48 | MenhirState44 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205 * _menhir_state * 'tv_location * Lexing.position) = Obj.magic _menhir_stack in
        (_menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) : 'freshtv206)
    | MenhirState99 | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211 * _menhir_state * 'tv_location * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv207 * _menhir_state * 'tv_location * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONST_INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEW ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv208)
        | AND | DIV | DOT | EQUAL | GE | GT | LB | LE | LT | MINUS | MOD | NEQ | OR | PLUS | STAR ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv209 * _menhir_state * 'tv_location * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)) : 'freshtv212)
    | _ ->
        _menhir_fail ()

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_arguments = 
# 165 "SourceParser.mly"
              ( [] )
# 2101 "SourceParser.ml"
     in
    _menhir_goto_arguments _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_literal : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_literal -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv203) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_literal) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv201) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (lit : 'tv_literal) = _v in
    let (_startpos_lit_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_lit_ in
    let _v : 'tv_expression = 
# 149 "SourceParser.mly"
                                               ( Literal(lit)          )
# 2121 "SourceParser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv202)) : 'freshtv204)

and _menhir_goto_var_decls : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_var_decls -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv195 * _menhir_state * 'tv_struct_decls) * _menhir_state * 'tv_var_decls) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MAIN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv191) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BEGIN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv192)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv193 * _menhir_state * 'tv_struct_decls) * _menhir_state * 'tv_var_decls) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)) : 'freshtv196)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv199 * _menhir_state * 'tv_var_decl) * Lexing.position) * _menhir_state * 'tv_var_decls) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv197 * _menhir_state * 'tv_var_decl) * Lexing.position) * _menhir_state * 'tv_var_decls) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, vd), _endpos__2_), _, vds) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_var_decls = 
# 97 "SourceParser.mly"
                                   ( let id, ty = vd in
                                     Symb_Tbl.add id ty vds )
# 2165 "SourceParser.ml"
         in
        _menhir_goto_var_decls _menhir_env _menhir_stack _menhir_s _v) : 'freshtv198)) : 'freshtv200)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expression -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState22 | MenhirState99 | MenhirState96 | MenhirState89 | MenhirState85 | MenhirState24 | MenhirState25 | MenhirState28 | MenhirState29 | MenhirState37 | MenhirState39 | MenhirState41 | MenhirState68 | MenhirState52 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState58 | MenhirState54 | MenhirState56 | MenhirState44 | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        (_menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) : 'freshtv176)
    | MenhirState71 | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv177 * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONST_INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEW ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RP ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv178)
        | AND | DIV | DOT | EQUAL | GE | GT | LB | LE | LT | MINUS | MOD | NEQ | OR | PLUS | STAR ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv179 * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)) : 'freshtv182)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv185 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv183 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, e, _startpos_e_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : 'tv_expression = 
# 151 "SourceParser.mly"
                                               ( e                     )
# 2239 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv184)) : 'freshtv186)
        | AND | DIV | DOT | EQUAL | GE | GT | LB | LE | LT | MINUS | MOD | NEQ | OR | PLUS | STAR ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv187 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)) : 'freshtv190)
    | _ ->
        _menhir_fail ()

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEAN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INTEGER ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST_INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LP ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "SourceParser.mly"
       (string)
# 2349 "SourceParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169 * _menhir_state * (
# 11 "SourceParser.mly"
       (string)
# 2361 "SourceParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CONST_BOOL _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CONST_INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LP ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEW ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RP ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv170)
    | AND | COMMA | DIV | DOT | END | EQUAL | GE | GT | LB | LE | LT | MINUS | MOD | NEQ | OR | PLUS | RB | RP | SEMI | SET | STAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state * (
# 11 "SourceParser.mly"
       (string)
# 2393 "SourceParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, id, _startpos_id_) = _menhir_stack in
        let _startpos = _startpos_id_ in
        let _v : 'tv_location = 
# 177 "SourceParser.mly"
                                             ( Identifier (Id id)  )
# 2400 "SourceParser.ml"
         in
        _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv172)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state * (
# 11 "SourceParser.mly"
       (string)
# 2410 "SourceParser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "SourceParser.mly"
       (int)
# 2418 "SourceParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv167) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (i : (
# 9 "SourceParser.mly"
       (int)
# 2428 "SourceParser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _v : 'tv_literal = 
# 172 "SourceParser.mly"
                ( Int i  )
# 2435 "SourceParser.ml"
     in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv168)

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "SourceParser.mly"
       (bool)
# 2442 "SourceParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv165) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (b : (
# 10 "SourceParser.mly"
       (bool)
# 2452 "SourceParser.ml"
    )) = _v in
    let (_startpos_b_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_b_ in
    let _v : 'tv_literal = 
# 173 "SourceParser.mly"
                ( Bool b )
# 2459 "SourceParser.ml"
     in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv166)

and _menhir_run12 : _menhir_env -> ('ttv_tail * _menhir_state * 'tv_typ) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv163 * _menhir_state * 'tv_typ)) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    ((let (_menhir_stack, _menhir_s, ty) = _menhir_stack in
    let _3 = () in
    let _2 = () in
    let _v : 'tv_typ = 
# 112 "SourceParser.mly"
                 ( TypArray ty  )
# 2475 "SourceParser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv164)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_var_decls = 
# 96 "SourceParser.mly"
                                   ( Symb_Tbl.empty )
# 2489 "SourceParser.ml"
     in
    _menhir_goto_var_decls _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEAN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INTEGER ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_goto_fields_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_fields_decl -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv149 * _menhir_state * 'tv_typed_ident) * Lexing.position) * _menhir_state * 'tv_fields_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv147 * _menhir_state * 'tv_typed_ident) * Lexing.position) * _menhir_state * 'tv_fields_decl) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, tid), _endpos__2_), _, f) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_fields_decl = 
# 92 "SourceParser.mly"
                                       ( tid :: f )
# 2524 "SourceParser.ml"
         in
        _menhir_goto_fields_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv148)) : 'freshtv150)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv161 * _menhir_state) * (
# 11 "SourceParser.mly"
       (string)
# 2532 "SourceParser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_fields_decl) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv157 * _menhir_state) * (
# 11 "SourceParser.mly"
       (string)
# 2542 "SourceParser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_fields_decl) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv155 * _menhir_state) * (
# 11 "SourceParser.mly"
       (string)
# 2549 "SourceParser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_fields_decl) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), id, _startpos_id_), _endpos__3_), _, f) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_struct_decl = 
# 84 "SourceParser.mly"
                                              ( id, { fields = f } )
# 2558 "SourceParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv153) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_struct_decl) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv151 * _menhir_state * 'tv_struct_decl) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | STRUCT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | MAIN | VAR ->
                _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109) : 'freshtv152)) : 'freshtv154)) : 'freshtv156)) : 'freshtv158)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv159 * _menhir_state) * (
# 11 "SourceParser.mly"
       (string)
# 2585 "SourceParser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_fields_decl) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)) : 'freshtv162)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_typ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState18 | MenhirState4 | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv131 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "SourceParser.mly"
       (string)
# 2608 "SourceParser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv129 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            let (id : (
# 11 "SourceParser.mly"
       (string)
# 2617 "SourceParser.ml"
            )) = _v in
            let (_startpos_id_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, ty) = _menhir_stack in
            let _v : 'tv_typed_ident = 
# 106 "SourceParser.mly"
                    ( id, ty )
# 2624 "SourceParser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv127) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_typed_ident) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState9 | MenhirState4 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv113 * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | SEMI ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv109 * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    ((let _menhir_stack = (_menhir_stack, _endpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | BOOLEAN ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9
                    | IDENT _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | INTEGER ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9
                    | END ->
                        _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) MenhirState9
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv110)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv111 * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)) : 'freshtv114)
            | MenhirState18 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv125 * _menhir_state) * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv123 * _menhir_state) * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _, tid) = _menhir_stack in
                let _1 = () in
                let _v : 'tv_var_decl = 
# 102 "SourceParser.mly"
                       ( tid )
# 2675 "SourceParser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv121) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_var_decl) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv119 * _menhir_state * 'tv_var_decl) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | SEMI ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv115 * _menhir_state * 'tv_var_decl) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    ((let _menhir_stack = (_menhir_stack, _endpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | VAR ->
                        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | MAIN ->
                        _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107) : 'freshtv116)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv117 * _menhir_state * 'tv_var_decl) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)) : 'freshtv120)) : 'freshtv122)) : 'freshtv124)) : 'freshtv126)
            | _ ->
                _menhir_fail ()) : 'freshtv128)) : 'freshtv130)) : 'freshtv132)
        | LB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv133 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RB ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11) : 'freshtv134)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv135 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)) : 'freshtv138)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv145 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LB ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv139 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CONST_INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LP ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEW ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28) : 'freshtv140)
        | AND | COMMA | DIV | DOT | END | EQUAL | GE | GT | LE | LT | MINUS | MOD | NEQ | OR | PLUS | RB | RP | SEMI | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv141 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, ty) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : 'tv_expression = 
# 157 "SourceParser.mly"
              ( match ty with TypStruct(id) -> NewRecord(id) | _ -> assert false )
# 2772 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv142)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv143 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)) : 'freshtv146)
    | _ ->
        _menhir_fail ()

and _menhir_goto_prog : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 47 "SourceParser.mly"
      (SourceLocalisedAST.program)
# 2788 "SourceParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv107) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 47 "SourceParser.mly"
      (SourceLocalisedAST.program)
# 2797 "SourceParser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv105) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 47 "SourceParser.mly"
      (SourceLocalisedAST.program)
# 2805 "SourceParser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv106)) : 'freshtv108)

and _menhir_goto_struct_decls : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_struct_decls -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state * 'tv_struct_decls) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | VAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | MAIN ->
            _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv100)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv103 * _menhir_state * 'tv_struct_decl) * _menhir_state * 'tv_struct_decls) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101 * _menhir_state * 'tv_struct_decl) * _menhir_state * 'tv_struct_decls) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, sd), _, sds) = _menhir_stack in
        let _v : 'tv_struct_decls = 
# 75 "SourceParser.mly"
                                   ( let id, s_info = sd in
                                     Symb_Tbl.add id s_info sds )
# 2837 "SourceParser.ml"
         in
        _menhir_goto_struct_decls _menhir_env _menhir_stack _menhir_s _v) : 'freshtv102)) : 'freshtv104)
    | _ ->
        _menhir_fail ()

and _menhir_reduce25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_fields_decl = 
# 91 "SourceParser.mly"
                                       ( []       )
# 2848 "SourceParser.ml"
     in
    _menhir_goto_fields_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv97) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_typ = 
# 110 "SourceParser.mly"
                 ( TypInt       )
# 2862 "SourceParser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv98)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "SourceParser.mly"
       (string)
# 2869 "SourceParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv95) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (id : (
# 11 "SourceParser.mly"
       (string)
# 2879 "SourceParser.ml"
    )) = _v in
    let (_startpos_id_ : Lexing.position) = _startpos in
    ((let _v : 'tv_typ = 
# 114 "SourceParser.mly"
                 ( TypStruct id )
# 2885 "SourceParser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv96)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv93) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_typ = 
# 111 "SourceParser.mly"
                 ( TypBool      )
# 2899 "SourceParser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv94)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state * 'tv_struct_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv13 * _menhir_state * 'tv_var_decl) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv15 * _menhir_state * 'tv_localised_instruction * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * 'tv_location * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv19 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position)) * _menhir_state * 'tv_block)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv21 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv23 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv27 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv35 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * 'tv_localised_expression * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state * 'tv_localised_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * (
# 11 "SourceParser.mly"
       (string)
# 3031 "SourceParser.ml"
        ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv65 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv76)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state * 'tv_struct_decls) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * 'tv_typ)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * 'tv_typed_ident) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv85 * _menhir_state) * (
# 11 "SourceParser.mly"
       (string)
# 3099 "SourceParser.ml"
        ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_startpos__1_ : Lexing.position) = _startpos in
        ((let _1 = () in
        let _v : (
# 47 "SourceParser.mly"
      (SourceLocalisedAST.program)
# 3118 "SourceParser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 59 "SourceParser.mly"
        ( let pos = _startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum pos.pos_cnum
          in
          failwith message )
# 3127 "SourceParser.ml"
         in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v) : 'freshtv88)) : 'freshtv90)) : 'freshtv92)

and _menhir_reduce44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_struct_decls = 
# 73 "SourceParser.mly"
                                   ( Symb_Tbl.empty )
# 3136 "SourceParser.ml"
     in
    _menhir_goto_struct_decls _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "SourceParser.mly"
       (string)
# 3152 "SourceParser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BEGIN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv3 * _menhir_state) * (
# 11 "SourceParser.mly"
       (string)
# 3164 "SourceParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEAN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INTEGER ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | END ->
                _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv4)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv5 * _menhir_state) * (
# 11 "SourceParser.mly"
       (string)
# 3190 "SourceParser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)) : 'freshtv8)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)

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
# 47 "SourceParser.mly"
      (SourceLocalisedAST.program)
# 3217 "SourceParser.ml"
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
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STRUCT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MAIN | VAR ->
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 220 "/usr/share/menhir/standard.mly"
  


# 3249 "SourceParser.ml"
