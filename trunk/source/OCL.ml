(*
 * ObjectDiagram.ml -- Definition of the abstract syntax of OCL.
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

(* Abstract syntax of OCL. *)

open ObjectDiagram;;

(** Define a type specification as they occur in our extended
    type checker  *)
type
  ocltypespec =
    TypeError (** Indicates a type checking error *)
  | Name of string (** A simple type name, e.g. Integer, Boolean, ... *)
  | Application of string * ocltypespec (** The application of an ocltypespec
                            to the parameterized type described by string *)
  | Variable of string (** A type variable *)
  | Union of ocltypespec list (** A union type *)
  | Intersection of ocltypespec list (** An intersection type *)
  ;;

(** Define the data type of the abstract syntax of OCL as used in the thesis.

    Contrary to the OCL meta model we do not use inheritance here, but
    a purely functional style. *)
type
    oclast =
      Value of value
    | Identifier of string
    | Collection of string * oclast list
    | If of oclast * oclast * oclast
    | AttributeCall of oclast * string
    | OperationCall of oclast * string * oclast list
    | CollectionCall of oclast * string * oclast list
    | Iterate of oclast * string * oclvardecl list * oclvardecl * oclast
    | Let of string * oclast list * oclast
    | Error
  and
    oclvardecl = { name: string; typespec: ocltypespec; init: oclast }
  ;;

(** The type checker

    For the case of allInstances, we have to make sure that the callee
    is really a type.  The abstract syntax assumes that the expression
    denoting the callee is really a general expression.  Therefore, we
    match on Variable for the callee argument, but we have to assert
    that the name of the variable coincides with the name of a
    class in the class diagram or a data type name.  *)
let rec ocltypechecker cd env (expression: oclast) =
  match expression with
    Value Null -> Name "OclAny"
  | Value Reference r -> TypeError
  | Value Boolean b -> Name "Boolean"
  | Value Integer i -> Name "Integer"
  | Value Real f -> Name "Real"
  | Value String s -> Name "String"
  | Value Bag b ->
      Application ("Bag", Union (List.map (ocltypechecker cd env)
				   (List.map (function x -> Value x) b)))
  | Value Sequence b ->
      Application ("Sequence", Union (List.map (ocltypechecker cd env)
					(List.map (function x -> Value x) b)))
  | Value Set b ->
      Application ("Set", Union (List.map (ocltypechecker cd env)
				   (List.map (function x -> Value x) b)))
  | Identifier i -> env i
  | Collection (n, e) ->
      Application (n, Union (List.map (ocltypechecker cd env) e))
  | If (c, e, f) ->
        (match ocltypechecker cd env c with
             Name "Boolean" -> Union [
                 ocltypechecker cd env e;
                 ocltypechecker cd env f]
           | _ -> TypeError)
  | AttributeCall (c, n) ->
      let ct = ocltypechecker cd env c
      in
      TypeError (* fetch () n *)
  | OperationCall (Identifier i, "allInstances", []) ->
        if true (* i is a type name *) then
            (Application ("Set", (Name i)))
        else
            TypeError
  | OperationCall (callee, "flatten", []) ->
      let calleetype = ocltypechecker cd env callee
      in ( match calleetype with
               Application (n1, Application(n2, t2)) -> Application (n1, t2)
             | t -> t )
  | OperationCall (callee, name, args) ->
        let calleetype = ocltypechecker cd env callee
        and argstype = List.map (ocltypechecker cd env) args
        in (match calleetype with
                TypeError -> TypeError
              | Application (n, a) ->
                  (* Here we have the map semantics. *)
                  Application (n, TypeError)
              | t -> TypeError )
                  (* Here we have the normal call semantics. *)
  | CollectionCall(c, n, a) -> TypeError
  | Iterate (c, n, d, a, e) ->
        let ct = ocltypechecker cd env c
        in ocltypechecker cd ((* XXX: update *) env) e
  | Error -> TypeError
  | _ -> assert false
  ;;

(** This function defines an interpreter for OCL constraints.

    The interpreter may not be very robust, and we have not implemented
    oclIsUndefined and friends, because these only cause problems.

    The interpreter is also only a proof of concept.  In order to be
    useful, the notion of a state has to be correctly implemented.

    Other than this, we believe the program to be correct.
    @param cd   Represents the class diagram in which the constraint is
                interpreted.
    @param cs   Represents the current state in which the constraint is
                interpreted.
    @param ps   Represents the previous state in which the constraint is
                interpreted.
    @param env  Represents the environment, which binds local variable names
                to their expressions, in which the constraint is evaluated.
    @param expr The expression to interpret.

    @return     A reduced expression representing the semantic value.
                The value may be Error, which means, that the constraint
                was probably not well-typed.  *)
let rec oclinterpreter cd cs ps env expr =
  match expr with
      Value v -> Value v
    | Identifier i -> env i
    | Collection (n, v) ->
	Collection (n, List.map (oclinterpreter cd cs ps env) v)
    | If (c, e, f) ->
	(match oclinterpreter cd cs ps env c with
             Value Boolean true -> oclinterpreter cd cs ps env e
           | Value Boolean false -> oclinterpreter cd cs ps env f
           | _ -> Error)
    | AttributeCall (c, n) ->
	( match oclinterpreter cd cs ps env c with
              Value Reference r ->
		Value (Hashtbl.find (List.nth cs.objects r).attributes n)
            | _ -> Error)
    | OperationCall (c, n, args) ->
	let vc = oclinterpreter cd cs ps env c
	and va = List.map (oclinterpreter cd cs ps env) args
	in (match vc with
		Value Boolean b -> Error
              | Value Integer i -> Error
              | Value Real r -> Error
              | Value String s -> Error
              | Collection (n, v) -> Error
              | _ -> Error)
    | CollectionCall (c, n, args) ->
	let va = List.map (oclinterpreter cd cs ps env) args
	in (match oclinterpreter cd cs ps env c with
		Collection ("Bag", values) -> Error
              | Collection ("Sequence", values) -> Error
              | Collection ("Set", values) -> Error
              | v -> oclinterpreter cd cs ps env
                  (CollectionCall (Collection ("Set", [v]), n, va)))
    | _ -> Error
;;





(* Implementation of OCL parsers.

   We do not use a table driven parser, because we need actually
   two parsers, but a table driven parser (as generated by ocamlyacc)
   only offers one entry point.

   Also, a stream based parser is a bit more tricky, because the
   grammar of OCL is in LL(2), so we need to be able to carry two
   tokens of look-ahead for parsing. *)

(** This provides the lexer context.

    We only store the line, perhaps more will follow?  *)

type 'a lexer_context = {
  stream: 'a Stream.t;
  mutable line: int;
};;

type ocltoken =
    Keyword of string * int
  | Identifier of string * int
  | Int of int * int
  | Float of float * int
  | Str of string * int
  | Bang of int
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

exception BadToken

(** Create a lexer for OCL.

    The lexer will turn a stream of characters (as obtained from a string or
    file) into a stream of tokens. *)
let lex input =
  let initial_buffer = String.create 256 in
  let buffer = ref initial_buffer in
  let bufpos = ref 0 in
  let reset_buffer () = buffer := initial_buffer; bufpos := 0
  and store c =
    if !bufpos >= String.length !buffer then
      begin
	let newbuffer = String.create (2 * !bufpos)
	in
	  String.blit !buffer 0 newbuffer 0 !bufpos;
	  buffer := newbuffer
      end;
    String.set !buffer !bufpos c;
    incr bufpos
  and get_string () =
    let s = String.sub !buffer 0 !bufpos in buffer := initial_buffer; s
  and keyword_table = Hashtbl.create 29
  in
    List.iter (fun s -> Hashtbl.add keyword_table s s)
      [ "and"; "context"; "def"; "derive"; "else"; "endif"; "endpackage";
	"false"; "if"; "implies"; "init"; "inv"; "in"; "iterate"; "let";
	"not"; "or"; "package"; "post"; "pre"; "then"; "true"; "Tuple";
	"xor" ];
    let identifier_or_keyword id line =
      try Keyword ((Hashtbl.find keyword_table id), line)
      with Not_found -> Identifier(id, line)
    in
    let rec next_token (context : 'a lexer_context) =
      match Stream.peek context.stream with
	  Some (' ' | '\009' | '\026' | '\012') ->
	    Stream.junk context.stream;
	    next_token context
	| Some ( '\013' | '\010' ) ->
	    Stream.junk context.stream;
	    context.line <- context.line + 1;
	    next_token context
	| Some ('A'..'Z' | 'a'..'z' | '_' | '\192'..'\255' as c) ->
	    Stream.junk context.stream;
	    reset_buffer();
	    store c;
	    parse_identifier_or_keyword
	| Some ('0'..'9' as c) ->
	    Stream.junk context.stream;
	    reset_buffer();
	    store c;
	    parse_number context
	| Some '!' -> Stream.junk context.stream; Bang context.line
	| Some '"' -> assert false;
	| Some '#' -> Stream.junk context.stream; Hash context.line
	| Some '$' -> Stream.junk context.stream; Dollar context.line
	| Some '%' -> Stream.junk context.stream; Percent context.line
	| Some '&' -> Stream.junk context.stream; Ampersand context.line
	| Some '(' -> Stream.junk context.stream; LParen context.line
	| Some ')' -> Stream.junk context.stream; RParen context.line
	| Some '*' -> Stream.junk context.stream; Mult context.line
	| Some '+' -> Stream.junk context.stream; Plus context.line
	| Some ',' -> Stream.junk context.stream; Comma context.line
	| Some '\'' ->
	    Stream.junk context.stream;
	    reset_buffer();
	    parse_string context
	| Some '-' ->
	    Stream.junk context.stream;
	    (match Stream.peek context.stream with
		 Some '-' -> Stream.junk context.stream; parse_comment context
	       | Some '>' -> Stream.junk context.stream; Arrow context.line
	       | _ -> Minus context.line)
	| Some '.' -> 
	    Stream.junk context.stream;
	    (match Stream.peek context.stream with
		 Some '.' -> Stream.junk context.stream; DotDot context.line
	       | _ -> Dot context.line)
	| Some '/' -> Stream.junk context.stream; Div context.line
	| Some ':' -> 
	    Stream.junk context.stream;
	    (match Stream.peek context.stream with
		 Some ':' ->
		   Stream.junk context.stream;
		   DoubleColon context.line
	       | Some '=' -> Stream.junk context.stream; Assign context.line
	       | _ -> Colon context.line)
	| Some ';' -> Stream.junk context.stream; Semicolon context.line
	| Some '<' ->
	    Stream.junk context.stream;
	    (match Stream.peek context.stream with
		 Some '<' -> Stream.junk context.stream; assert false
	       | Some '=' -> Stream.junk context.stream; LessEq context.line
	       | Some '>' -> Stream.junk context.stream; NotEq context.line
	       | _ -> Less context.line)
	| Some '>' -> 
	    Stream.junk context.stream;
	    (match Stream.peek context.stream with
	       | Some '=' -> Stream.junk context.stream; GreaterEq context.line
	       | Some '>' -> Stream.junk context.stream; NotEq context.line
	       | _ -> Greater context.line)
	| Some '?' -> Question context.line
	| Some '@' -> At context.line
	| Some '[' -> LBracket context.line
	| Some ']' -> RBracket context.line
	| Some '^' -> 
	    Stream.junk context.stream;
	    (match Stream.peek context.stream with
		 Some '^' -> Stream.junk context.stream; HatHat context.line
	       | _ -> Hat context.line)
	| Some '{' -> Stream.junk context.stream; LBrace context.line
	| Some '|' -> Stream.junk context.stream; Bar context.line
	| Some '}' -> Stream.junk context.stream; RBrace context.line
	| Some '~' -> Stream.junk context.stream; Tilde context.line
	| _ -> assert false
    and parse_identifier_or_keyword =
      Keyword("", 0)
    and parse_string context =
      match Stream.peek context.stream with
	  Some('\'') ->
	    Stream.junk context.stream;
	    Str(get_string(), context.line)
	| Some '\\' ->
	    let c = try parse_escape context with
		Stream.Failure -> raise (Stream.Error "")
	    in let s = context.stream in store c; parse_string context
	| Some c ->
	    Stream.junk context.stream;
	    parse_string context
	| _ -> raise Stream.Failure
    and parse_escape context =
      match Stream.peek context.stream with
	| Some 'n' -> Stream.junk context.stream; '\n'
	| Some 'r' -> Stream.junk context.stream; '\t'
	| Some 't' -> Stream.junk context.stream; '\r'
	| Some ('0'..'9' as c1) ->
	    Stream.junk context.stream;
	    begin match Stream.peek context.stream with
		Some ('0'..'9' as c2) ->
		  Stream.junk context.stream;
		  begin match Stream.peek context.stream with
		      Some ('0'..'9' as c3) ->
			Stream.junk context.stream;
			Char.chr ((Char.code c1 - 48) * 100 +
				    (Char.code c2 -48) * 10 + (Char.code c3))
		    | _ -> raise (Stream.Error "")
		  end
	      | _ -> raise (Stream.Error "")
	    end
	| Some c -> Stream.junk context.stream; c
	| _ -> raise Stream.Failure
    and parse_number context =
      match Stream.peek context.stream with
	  Some ('0'..'9' as c) ->
	    Stream.junk context.stream;
	    store c;
	    parse_number context
	| Some '.' ->
	    Stream.junk context.stream;
	    store '.';
	    parse_decimal_part context
	| Some ('e' | 'E') ->
	    Stream.junk context.stream;
	    store 'e';
	    parse_exponent_part context
	| _ -> Int (int_of_string (get_string ()), context.line)
    and parse_decimal_part context =
      match Stream.peek context.stream with
	  Some ('0'..'9' as c) ->
	    Stream.junk context.stream;
	    store c;
	    parse_decimal_part context
	| Some ('e' | 'E') ->
	    Stream.junk context.stream;
	    store 'e';
	    parse_exponent_part context
	| _ -> Float (float_of_string (get_string ()), context.line)
    and parse_exponent_part context =
      match Stream.peek context.stream with
	  Some ('+' | '-' as c) ->
	    Stream.junk context.stream;
	    store c;
	    parse_end_exponent_part context
	| _ -> parse_end_exponent_part context
    and parse_end_exponent_part context =
      match Stream.peek context.stream with
	  Some ('0'..'9' as c) ->
	    Stream.junk context.stream;
	    store c;
	    parse_end_exponent_part context
	| _ -> Float (float_of_string (get_string ()), context.line)
    and parse_comment context =
      match Stream.peek context.stream with
	  Some('\010' | '\013') -> next_token context
	| Some _ -> Stream.junk context.stream; parse_comment context
	| _ -> raise Stream.Failure
    in next_token { stream = input; line = 1 }
;;

let parse_expression context =
  ()
;;


let parse_file context =
  parse_expression context
;;

(** Parse an expression from a string *)
let expression_from_string s =
  parse_expression { stream = Stream.of_string s; line = 1 }
;;

(** Parse an expression from a file *)
let expression_from_file name =
  parse_expression { stream = Stream.of_channel (open_in name); line = 1 }
;;

(** Parse an OCL file from a string *)
let from_string s =
  parse_file { stream = Stream.of_string s; line = 1 }
;;

(** Parse an OCL file from a file *)
let from_file name =
  parse_file { stream = Stream.of_channel (open_in name); line = 1 }
;;
