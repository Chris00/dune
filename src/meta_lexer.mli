type token =
  | Name   of Sexp.Atom.t
  | String of string
  | Minus
  | Lparen
  | Rparen
  | Comma
  | Equal
  | Plus_equal
  | Eof

val token : Lexing.lexbuf -> token
