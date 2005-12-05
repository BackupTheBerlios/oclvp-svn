(*
 * Lexer.mli -- Interface to the OCL lexer.
 *
 * This file is part of oclvp
 *
 * Written and Copyright (c) 2005 by Marcel Kyas
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

(** Interface to the OCL lexer.

    The lexer is also used by various other parsers. *)

(** The tokens returned by the lexer.  If the token has a semantic
    value, then it is the first component of the constructor.  The last
    one, an integer, refers to the line number of the token. *)
type token =
    Keyword of string * int
  | Id of string * int
  | Int of int * int
  | Float of float * int
  | Str of string * int
  | Bang of int
  | Quote of int
  | Hash of int
  | Dollar of int
  | Percent of int
  | Ampersand of int
  | LParen of int
  | RParen of int
  | Mult of int
  | Plus of int
  | Comma of int
  | Minus of int
  | Arrow of int
  | Dot of int
  | DotDot of int
  | Div of int
  | Colon of int
  | DoubleColon of int
  | Assign of int
  | Semicolon of int
  | Less of int
  | LessEq of int
  | NotEq of int
  | Equals of int
  | Greater of int
  | GreaterEq of int
  | Question of int
  | At of int
  | LBracket of int
  | RBracket of int
  | Hat of int
  | HatHat of int
  | LBrace of int
  | Bar of int
  | RBrace of int
  | Tilde of int

(** Exception raised when the lexer or parser encounters the end of
    a file in an unexprected place.  The string argument described the
    place where the end of file has been encountered. *)
exception Eof of string

(** This exception is raised if a token has been encountered which the
    parser does not expect. The first argument to the constructor
    refers to the unexpected token, the second to the line in which it
    has been found, and the third may be used for some explenation.
    This explenation is usually the follow-set, that is, the set of
    tokens expected instead. *)
exception BadToken of string * int * string

val get_token_line: token -> int
  (** Extract the line of a token. *)

val get_token_name: token -> string
  (** Return a string representing the kind of the token and its
      semantic value if applicable. *)

val lexer: char Stream.t -> token Stream.t
(** Create a lexer for OCL.

    The lexer turns a stream of characters (as obtained from a string or
    file) into a stream of tokens.

    The tokenizer filters out all comments.  *)
