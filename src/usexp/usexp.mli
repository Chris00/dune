(** Parsing of s-expressions *)

module Atom : sig
  type t = string
  (** Acceptable atoms are composed of chars in the range [' ' .. '~']
     and must be nonempty. *)

  val valid : string -> bool
  (** [valid s] checks that [s] respects the constraints to be an atom. *)

  val of_string : string -> t
  (** Convert a string to an atom.  If the string contains invalid
     characters, raise [Invalid_argument]. *)

  val uncapitalize : t -> t
  (** [uncapitalize s] make the first char of [s] lowercase. *)

  val compare : t -> t -> int
end

module Loc : sig
  type t =
    { start : Lexing.position
    ; stop  : Lexing.position
    }
end

(** The S-expression type.  An [Atom.t] value will necessarily satisfy
   the predicate [Atom.valid]. *)
type t = private
  | Atom of Atom.t
  | Quoted_string of string
  | List of t list

val atom : string -> t
(** [atom s] construct an atom from [s].
    @raise Invalid_argument if [Atom.valid s] is [false].  *)

val atom_of_int64 : Int64.t -> t

val quoted_string : string -> t

val atom_or_quoted_string : string -> t
(** [atom_or_quoted_string s] returns an [Atom] if [s] is a valid atom
   or a [Quoted_string] otherwise. *)

val list : t list -> t

(** Serialize a S-expression *)
val to_string : t -> string

(** Serialize a S-expression using indentation to improve readability *)
val pp : Format.formatter -> t -> unit

(** Same as [pp], but split long strings. The formatter must have been
    prepared with [prepare_formatter]. *)
val pp_split_strings : Format.formatter -> t -> unit

(** Prepare a formatter for [pp_split_strings]. Additionaly the
    formatter escape newlines when the tags "makefile-action" or
    "makefile-stuff" are active. *)
val prepare_formatter : Format.formatter -> unit

(** Abstract syntax tree *)
module Ast : sig
  type sexp = t
  type t = private
    | Atom of Loc.t * Atom.t
    | Quoted_string of Loc.t * string
    | List of Loc.t * t list

  val atom : Loc.t -> string -> t
  (** [atom loc s] construct an [Atom] from [s].
      @raise Invalid_argument if [s] does not satisfy [Atom.valid s]. *)

  val list : Loc.t -> t list -> t
  (** [list loc l] construct a sexp from a list of sexp.  *)

  val loc : t -> Loc.t

  val remove_locs : t -> sexp

  module Token : sig
    type t =
      | Atom   of Loc.t * string
      | String of Loc.t * string
      | Lparen of Loc.t
      | Rparen of Loc.t
  end

  val tokenize : t -> Token.t list
end with type sexp := t

val add_loc : t -> loc:Loc.t -> Ast.t

module Parser : sig
  module Error : sig
    type t

    val position : t -> Lexing.position
    val message  : t -> string
  end

  (** Exception raised in case of a parsing error *)
  exception Error of Error.t

  module Mode : sig
    type sexp = t
    type 'a t =
      | Single : Ast.t t
      | Many   : Ast.t list t
  end with type sexp := t

  module Stack : sig
    (** Parser stack. The stack is not in [state] for optimization purposes. *)
    type t

    val empty : t
  end

  type 'a t

  (** Create a new parser state. [fname] is the filename the input is from. *)
  val create : fname:string -> mode:'a Mode.t -> 'a t

  (** Feed one character to the parser. In case of error, it raises [Parse_error] *)
  val feed : _ t -> char -> Stack.t -> Stack.t

  (** Instruct the parser that the end of input was reached. In case of error, it raises
      [Parse_error] *)
  val feed_eoi : 'a t -> Stack.t -> 'a

  (** {3 Convenience functions} *)

  val feed_string    : _ t -> string                       -> Stack.t -> Stack.t
  val feed_substring : _ t -> string -> pos:int -> len:int -> Stack.t -> Stack.t
  val feed_bytes     : _ t -> bytes                        -> Stack.t -> Stack.t
  val feed_subbytes  : _ t -> bytes -> pos:int -> len:int  -> Stack.t -> Stack.t
end

val parse_string : fname:string -> mode:'a Parser.Mode.t -> string -> 'a
