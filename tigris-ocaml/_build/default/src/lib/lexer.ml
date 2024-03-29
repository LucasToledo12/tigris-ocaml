# 1 "src/lib/lexer.mll"
 
  module L = Lexing

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char

  let unterminated_comment loc =
    Error.error loc "unterminated comment"

  let unterminated_string loc =
    Error.error loc "unterminated string"

  let illegal_escape loc sequence =
    Error.error loc "illegal escape sequence '%s' in string literal" sequence

  let set_filename lexbuf fname =
    lexbuf.L.lex_curr_p <-  
      { lexbuf.L.lex_curr_p with L.pos_fname = fname }

  (* a string buffer to accumulate characters when scanning string literals *)
  let string_buffer = Buffer.create 16

  (* helper function to update new line counting while scanning string literals *)
  let str_incr_linenum str lexbuf =
    String.iter (function '\n' -> L.new_line lexbuf | _ -> ()) str
  
  let append_char str ch =
    str ^ (String.make 1 (Char.chr ch))

# 34 "src/lib/lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\220\255\221\255\223\255\224\255\002\000\003\000\230\255\
    \231\255\232\255\233\255\234\255\235\255\236\255\237\255\238\255\
    \005\000\240\255\241\255\000\000\001\000\000\000\000\000\000\000\
    \002\000\002\000\005\000\252\255\019\000\254\255\003\000\244\255\
    \251\255\002\000\003\000\250\255\009\000\002\000\019\000\249\255\
    \243\255\016\000\014\000\022\000\248\255\247\255\024\000\002\000\
    \019\000\246\255\011\000\245\255\014\000\242\255\222\255\227\255\
    \229\255\225\255\043\000\044\000\095\000\255\255\031\000\090\000\
    \249\255\250\255\251\255\252\255\253\255\254\255\248\255\146\000\
    \247\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\029\000\027\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \016\000\255\255\255\255\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\255\255\002\000\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\009\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\000\000\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000\255\255\000\000\
    \000\000\255\255\255\255\000\000\255\255\255\255\255\255\000\000\
    \000\000\255\255\255\255\255\255\000\000\000\000\255\255\255\255\
    \255\255\000\000\255\255\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\059\000\059\000\255\255\000\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\030\000\029\000\000\000\030\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \030\000\000\000\027\000\030\000\000\000\009\000\004\000\000\000\
    \018\000\017\000\011\000\013\000\015\000\012\000\000\000\010\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\016\000\014\000\006\000\007\000\005\000\057\000\
    \055\000\056\000\054\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\061\000\255\255\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
    \000\000\052\000\021\000\048\000\022\000\024\000\050\000\034\000\
    \041\000\026\000\033\000\032\000\020\000\040\000\037\000\045\000\
    \036\000\035\000\046\000\031\000\025\000\038\000\019\000\023\000\
    \039\000\042\000\043\000\044\000\003\000\047\000\049\000\051\000\
    \053\000\068\000\000\000\000\000\000\000\000\000\000\000\060\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\000\000\069\000\000\000\063\000\000\000\000\000\
    \000\000\064\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\000\000\066\000\000\000\000\000\
    \000\000\065\000\000\000\067\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\030\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\030\000\255\255\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
    \006\000\006\000\016\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\058\000\059\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\019\000\000\000\047\000\000\000\000\000\020\000\033\000\
    \023\000\000\000\025\000\026\000\000\000\036\000\024\000\022\000\
    \024\000\034\000\021\000\026\000\000\000\037\000\000\000\000\000\
    \038\000\041\000\042\000\043\000\000\000\046\000\048\000\050\000\
    \052\000\060\000\255\255\255\255\255\255\255\255\255\255\058\000\
    \059\000\255\255\255\255\255\255\255\255\255\255\255\255\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\255\255\060\000\255\255\060\000\255\255\255\255\
    \255\255\060\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\255\255\060\000\255\255\255\255\
    \255\255\060\000\255\255\060\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\058\000\059\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 38 "src/lib/lexer.mll"
                  ( token lexbuf )
# 194 "src/lib/lexer.ml"

  | 1 ->
# 39 "src/lib/lexer.mll"
                  ( L.new_line lexbuf;
                    token lexbuf )
# 200 "src/lib/lexer.ml"

  | 2 ->
let
# 41 "src/lib/lexer.mll"
              lxm
# 206 "src/lib/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 41 "src/lib/lexer.mll"
                  ( INTEGER (int_of_string lxm) )
# 210 "src/lib/lexer.ml"

  | 3 ->
# 42 "src/lib/lexer.mll"
                  ( string lexbuf.L.lex_start_p lexbuf )
# 215 "src/lib/lexer.ml"

  | 4 ->
# 44 "src/lib/lexer.mll"
                  ( IF )
# 220 "src/lib/lexer.ml"

  | 5 ->
# 45 "src/lib/lexer.mll"
                  ( THEN )
# 225 "src/lib/lexer.ml"

  | 6 ->
# 46 "src/lib/lexer.mll"
                  ( ELSE )
# 230 "src/lib/lexer.ml"

  | 7 ->
# 47 "src/lib/lexer.mll"
                  ( WHILE )
# 235 "src/lib/lexer.ml"

  | 8 ->
# 48 "src/lib/lexer.mll"
                  ( DO )
# 240 "src/lib/lexer.ml"

  | 9 ->
# 49 "src/lib/lexer.mll"
                  ( BREAK )
# 245 "src/lib/lexer.ml"

  | 10 ->
# 50 "src/lib/lexer.mll"
                  ( LET )
# 250 "src/lib/lexer.ml"

  | 11 ->
# 51 "src/lib/lexer.mll"
                  ( IN )
# 255 "src/lib/lexer.ml"

  | 12 ->
# 52 "src/lib/lexer.mll"
                  ( END )
# 260 "src/lib/lexer.ml"

  | 13 ->
# 53 "src/lib/lexer.mll"
                  ( VAR )
# 265 "src/lib/lexer.ml"

  | 14 ->
# 54 "src/lib/lexer.mll"
                  ( LPAREN )
# 270 "src/lib/lexer.ml"

  | 15 ->
# 55 "src/lib/lexer.mll"
                  ( RPAREN )
# 275 "src/lib/lexer.ml"

  | 16 ->
# 56 "src/lib/lexer.mll"
                  ( COLON )
# 280 "src/lib/lexer.ml"

  | 17 ->
# 57 "src/lib/lexer.mll"
                  ( COMMA )
# 285 "src/lib/lexer.ml"

  | 18 ->
# 58 "src/lib/lexer.mll"
                  ( SEMI )
# 290 "src/lib/lexer.ml"

  | 19 ->
# 59 "src/lib/lexer.mll"
                  ( PLUS )
# 295 "src/lib/lexer.ml"

  | 20 ->
# 60 "src/lib/lexer.mll"
                  ( MINUS )
# 300 "src/lib/lexer.ml"

  | 21 ->
# 61 "src/lib/lexer.mll"
                  ( TIMES )
# 305 "src/lib/lexer.ml"

  | 22 ->
# 62 "src/lib/lexer.mll"
                  ( DIV )
# 310 "src/lib/lexer.ml"

  | 23 ->
# 63 "src/lib/lexer.mll"
                  ( MOD )
# 315 "src/lib/lexer.ml"

  | 24 ->
# 64 "src/lib/lexer.mll"
                  ( POW )
# 320 "src/lib/lexer.ml"

  | 25 ->
# 65 "src/lib/lexer.mll"
                  ( EQ )
# 325 "src/lib/lexer.ml"

  | 26 ->
# 66 "src/lib/lexer.mll"
                  ( NE )
# 330 "src/lib/lexer.ml"

  | 27 ->
# 67 "src/lib/lexer.mll"
                  ( LT )
# 335 "src/lib/lexer.ml"

  | 28 ->
# 68 "src/lib/lexer.mll"
                  ( LE )
# 340 "src/lib/lexer.ml"

  | 29 ->
# 69 "src/lib/lexer.mll"
                  ( GT )
# 345 "src/lib/lexer.ml"

  | 30 ->
# 70 "src/lib/lexer.mll"
                  ( GE )
# 350 "src/lib/lexer.ml"

  | 31 ->
# 71 "src/lib/lexer.mll"
                  ( AND )
# 355 "src/lib/lexer.ml"

  | 32 ->
# 72 "src/lib/lexer.mll"
                  ( OR )
# 360 "src/lib/lexer.ml"

  | 33 ->
# 73 "src/lib/lexer.mll"
                  ( ASSIGN )
# 365 "src/lib/lexer.ml"

  | 34 ->
# 74 "src/lib/lexer.mll"
                  ( EOF )
# 370 "src/lib/lexer.ml"

  | 35 ->
# 75 "src/lib/lexer.mll"
                  ( illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) )
# 375 "src/lib/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and string pos lexbuf =
   __ocaml_lex_string_rec pos lexbuf 58
and __ocaml_lex_string_rec pos lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 78 "src/lib/lexer.mll"
                       ( lexbuf.L.lex_start_p <- pos;
                         let text = Buffer.contents string_buffer in
                         Buffer.clear string_buffer;
                         STRING text )
# 390 "src/lib/lexer.ml"

  | 1 ->
# 82 "src/lib/lexer.mll"
                       ( Buffer.add_char string_buffer '\\';
                        string pos lexbuf )
# 396 "src/lib/lexer.ml"

  | 2 ->
# 84 "src/lib/lexer.mll"
                      ( Buffer.add_char string_buffer '\"';
                        string pos lexbuf )
# 402 "src/lib/lexer.ml"

  | 3 ->
# 86 "src/lib/lexer.mll"
                       ( Buffer.add_char string_buffer '\t';
                         string pos lexbuf )
# 408 "src/lib/lexer.ml"

  | 4 ->
# 90 "src/lib/lexer.mll"
                       ( Buffer.add_char string_buffer '\n';
                         string pos lexbuf )
# 414 "src/lib/lexer.ml"

  | 5 ->
# 92 "src/lib/lexer.mll"
                       ( Buffer.add_char string_buffer '\r';
                         string pos lexbuf )
# 420 "src/lib/lexer.ml"

  | 6 ->
# 94 "src/lib/lexer.mll"
                       ( Buffer.add_char string_buffer '\b';
                         string pos lexbuf )
# 426 "src/lib/lexer.ml"

  | 7 ->
let
# 98 "src/lib/lexer.mll"
                                                   i
# 432 "src/lib/lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2) in
# 98 "src/lib/lexer.mll"
                                                       ( Buffer.add_char string_buffer i;
                         string pos lexbuf )
# 437 "src/lib/lexer.ml"

  | 8 ->
let
# 100 "src/lib/lexer.mll"
                             x
# 443 "src/lib/lexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) (lexbuf.Lexing.lex_start_pos + 4) in
# 100 "src/lib/lexer.mll"
                                    ( Buffer.add_string string_buffer x; 
                         string pos lexbuf )
# 448 "src/lib/lexer.ml"

  | 9 ->
let
# 103 "src/lib/lexer.mll"
                   lxm
# 454 "src/lib/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 103 "src/lib/lexer.mll"
                       ( str_incr_linenum lxm lexbuf;
                         Buffer.add_string string_buffer lxm;
                         string pos lexbuf )
# 460 "src/lib/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_string_rec pos lexbuf __ocaml_lex_state

;;

