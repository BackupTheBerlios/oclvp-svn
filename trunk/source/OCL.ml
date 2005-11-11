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

let rec prettyprint_typespec t =
  match t with
      Name n -> n
    | Application (n, t) -> n ^ "(" ^ (prettyprint_typespec t) ^ ")"
    | Variable v -> "$" ^ v
    | Union l -> (prettyprint_union " ++ " l)
    | Intersection l -> (prettyprint_union " ** " l)
    | _ -> assert false
and prettyprint_union oper l =
  match l with
      [] -> assert false
    | a::[] -> (prettyprint_typespec a)
    | a::l -> (prettyprint_typespec a) ^ oper ^ (prettyprint_union oper l)





(** Define the data type of the abstract syntax of OCL as used in the thesis.

    Contrary to the OCL meta model we do not use inheritance here, but
    a purely functional style. *)
type
    oclast =
      Value of value
    | Identifier of string
    | Pathname of string list
    | Collection of string * oclast list
    | If of oclast * oclast * oclast
    | AttributeCall of oclast * string
    | OperationCall of oclast * string * oclast list
    | CollectionCall of oclast * string * oclast list
    | Iterate of oclast * string * oclvardecl list * oclvardecl * oclast
    | Let of (string * oclast) list * oclast
    | Error
  and
    oclvardecl = { name: string; typespec: ocltypespec; init: oclast option }
  ;;




let rec prettyprint_pathname path =
  match path with
      [] -> assert false
    | n::[] -> n
    | n::r -> n ^ "::" ^ (prettyprint_pathname r)




(** Print a tree. *)
let rec prettyprint tree =
  match tree with
      Value v -> ""
    | Identifier i -> i
    | Pathname path -> (prettyprint_pathname path)
    | Collection (s, l) -> s ^ "{" ^ (prettyprint_args l) ^ "}"
    | If (c, t, f) ->
	"if " ^ (prettyprint c) ^ " then " ^ (prettyprint t) ^ " else " ^
	  (prettyprint f) ^ " endif"
    | AttributeCall (s, a) -> (prettyprint s) ^ "." ^ a
    | OperationCall (s, m, a) ->
	(prettyprint s) ^ "." ^ m ^ "(" ^ (prettyprint_args a) ^ ")"
    | CollectionCall (s, m, a) ->
	(prettyprint s) ^ "->" ^ m ^ "(" ^ (prettyprint_args a) ^ ")"
    | Iterate (c, n, v, ac, arg) ->
	(prettyprint c) ^ "->" ^ n
    | Let (v, i) -> "let " ^ (prettyprint_lets v) ^ " in " ^ (prettyprint i)
    | Error -> assert false
and prettyprint_args args =
  match args with
      [] -> ""
    | e::[] -> (prettyprint e)
    | e::r -> (prettyprint e) ^ ", " ^ (prettyprint_args r)
and prettyprint_lets l =
  match l with
      [] -> assert false
    | (n,e)::[] -> n ^ " = " ^ (prettyprint e)
    | (n,e)::r -> n ^ " = " ^ (prettyprint e) ^ ", " ^ (prettyprint_lets r)
and prettyprint_decls l =
  match l with 
      [] -> assert false
    | d::[] -> (prettyprint_decl d)
    | d::r -> (prettyprint_decl d) ^ "," ^ (prettyprint_decls r)
and prettyprint_decl d =
  d.name ^ ": " ^ (prettyprint_typespec d.typespec) ^
    (match d.init with Some i -> " = " ^ (prettyprint i) | None -> "")





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

(** Enumerate the tokens used by the parser. *)
type ocltoken =
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
;;

exception BadToken

exception ParseError

exception Eof
;;

(** Create a lexer for OCL.

    The lexer will turn a stream of characters (as obtained from a string or
    file) into a stream of tokens. *)
let lexer input =
  let line = ref 0 in
  let initial_buffer = String.create 256 in
  let buffer = ref initial_buffer in
  let bufpos = ref 0 in
  let reset_buffer () = buffer := initial_buffer; bufpos := 0 in
  let store c =
    if !bufpos >= String.length !buffer then
      begin
	let newbuffer = String.create (2 * !bufpos)
	in
	  String.blit !buffer 0 newbuffer 0 !bufpos;
	  buffer := newbuffer
      end;
    String.set !buffer !bufpos c;
    incr bufpos in
  let get_string () =
    let s = String.sub !buffer 0 !bufpos in buffer := initial_buffer; s in
  let keyword_table = Hashtbl.create 29 in
    List.iter (fun s -> Hashtbl.add keyword_table s s)
      [ "and"; "context"; "def"; "derive"; "else"; "endif"; "endpackage";
	"false"; "if"; "implies"; "init"; "inv"; "in"; "iterate"; "let";
	"not"; "or"; "package"; "post"; "pre"; "then"; "true"; "Tuple";
	"xor" ];
    let identifier_or_keyword id =
      try Some (Keyword ((Hashtbl.find keyword_table id), !line))
      with Not_found -> Some (Id (id, !line))
    in
    let rec next_token stream =
      match Stream.peek stream with
	  Some (' ' | '\009' | '\026' | '\012') ->
	    Stream.junk stream; next_token stream
	| Some ( '\013' | '\010' ) ->
	    Stream.junk stream; incr line; next_token stream 
	| Some ('A'..'Z' | 'a'..'z' | '_' | '\192'..'\255' as c) ->
	    Stream.junk stream; reset_buffer (); store c;
	    parse_identifier_or_keyword stream
	| Some ('0'..'9' as c) ->
	    Stream.junk stream; reset_buffer (); store c; parse_number stream
	| Some '!' -> Stream.junk stream; Some(Bang !line)
	| Some '"' -> Stream.junk stream; Some(Quote !line)
	| Some '#' -> Stream.junk stream; Some(Hash !line)
	| Some '$' -> Stream.junk stream; Some(Dollar !line)
	| Some '%' -> Stream.junk stream; Some(Percent !line)
	| Some '&' -> Stream.junk stream; Some(Ampersand !line)
	| Some '(' -> Stream.junk stream; Some(LParen !line)
	| Some ')' -> Stream.junk stream; Some(RParen !line)
	| Some '*' -> Stream.junk stream; Some(Mult !line)
	| Some '+' -> Stream.junk stream; Some(Plus !line)
	| Some ',' -> Stream.junk stream; Some(Comma !line)
	| Some '\'' -> Stream.junk stream; reset_buffer(); parse_string stream
	| Some '-' ->
	    Stream.junk stream;
	    begin
	      match Stream.peek stream with
		  Some '-' -> Stream.junk stream; parse_comment stream
		| Some '>' -> Stream.junk stream; Some(Arrow !line)
		| _ -> Some(Minus !line)
	    end
	| Some '.' -> 
	    Stream.junk stream;
	    begin
	      match Stream.peek stream with
		  Some '.' -> Stream.junk stream; Some(DotDot !line)
	       | _ -> Some(Dot !line)
	    end
	| Some '/' -> Stream.junk stream; Some(Div !line)
	| Some ':' -> 
	    Stream.junk stream;
	    begin
	      match Stream.peek stream with
		  Some ':' -> Stream.junk stream; Some(DoubleColon !line)
		| Some '=' -> Stream.junk stream; Some(Assign !line)
		| _ -> Some(Colon !line)
	    end
	| Some ';' -> Stream.junk stream; Some(Semicolon !line)
	| Some '<' ->
	    Stream.junk stream;
	    begin
	      match Stream.peek stream with
		  Some '<' -> Stream.junk stream; assert false
		| Some '=' -> Stream.junk stream; Some(LessEq !line)
		| Some '>' -> Stream.junk stream; Some(NotEq !line)
		| _ -> Some(Less !line)
	    end
	| Some '>' -> 
	    Stream.junk stream;
	    begin
	      match Stream.peek stream with
		| Some '=' -> Stream.junk stream; Some(GreaterEq !line)
		| Some '>' -> Stream.junk stream; Some(NotEq !line)
		| _ -> Some(Greater !line)
	    end
	| Some '?' -> Stream.junk stream; Some(Question !line)
	| Some '@' -> Stream.junk stream; Some(At !line)
	| Some '[' -> Stream.junk stream; Some(LBracket !line)
	| Some ']' -> Stream.junk stream; Some(RBracket !line)
	| Some '^' -> 
	    Stream.junk stream;
	    begin
              match Stream.peek stream with
		 Some '^' -> Stream.junk stream; Some(HatHat !line)
	       | _ -> Some(Hat !line)
            end
	| Some '{' -> Stream.junk stream; Some(LBrace !line)
	| Some '|' -> Stream.junk stream; Some(Bar !line)
	| Some '}' -> Stream.junk stream; Some(RBrace !line)
	| Some '~' -> Stream.junk stream; Some(Tilde !line)
	| _ -> raise Eof
    and parse_identifier_or_keyword stream =
      match Stream.peek stream with
	  Some ('0'..'9' | 'A'..'Z' | 'a'..'z' | '_' | '\192'..'\255' as c) ->
	    Stream.junk stream; store c; parse_identifier_or_keyword stream
	| _ -> identifier_or_keyword (get_string ())
    and parse_string stream =
      match Stream.peek stream with
	  Some('\'') -> Stream.junk stream; Some(Str(get_string(), !line))
	| Some '\\' ->
	    let c = try parse_escape stream with
		Stream.Failure -> raise (Stream.Error "")
	    in store c; parse_string stream
	| Some ('\010' | '\013') -> raise (Stream.Error "")
	| Some c -> Stream.junk stream; store c; parse_string stream
	| _ -> raise Stream.Failure
    and parse_escape stream =
      match Stream.peek stream with
	| Some 'n' -> Stream.junk stream; '\n'
	| Some 'r' -> Stream.junk stream; '\t'
	| Some 't' -> Stream.junk stream; '\r'
	| Some ('0'..'9' as c1) ->
	    Stream.junk stream;
	    begin match Stream.peek stream with
		Some ('0'..'9' as c2) ->
		  Stream.junk stream;
		  begin match Stream.peek stream with
		      Some ('0'..'9' as c3) ->
			Stream.junk stream;
			Char.chr ((Char.code c1 - 48) * 100 +
				    (Char.code c2 -48) * 10 + (Char.code c3))
		    | _ -> raise (Stream.Error "")
		  end
	      | _ -> raise (Stream.Error "")
	    end
	| Some c -> Stream.junk stream; c
	| _ -> raise Stream.Failure
    and parse_number stream =
      match Stream.peek stream with
	  Some ('0'..'9' as c) ->
	    Stream.junk stream;
	    store c;
	    parse_number stream
	| Some '.' -> Stream.junk stream; store '.'; parse_decimal_part stream
	| Some ('e' | 'E') ->
	    Stream.junk stream;
	    store 'e';
	    parse_exponent_part stream
	| _ -> Some(Int (int_of_string (get_string ()), !line))
    and parse_decimal_part stream =
      match Stream.peek stream with
	  Some ('0'..'9' as c) ->
	    Stream.junk stream;
	    store c;
	    parse_decimal_part stream
	| Some ('e' | 'E') ->
	    Stream.junk stream;
	    store 'e';
	    parse_exponent_part stream
	| _ -> Some(Float (float_of_string (get_string ()), !line))
    and parse_exponent_part stream =
      match Stream.peek stream with
	  Some ('+' | '-' as c) ->
	    Stream.junk stream;
	    store c;
	    parse_end_exponent_part stream
	| _ -> parse_end_exponent_part stream
    and parse_end_exponent_part stream =
      match Stream.peek stream with
	  Some ('0'..'9' as c) ->
	    Stream.junk stream;
	    store c;
	    parse_end_exponent_part stream
	| _ -> Some(Float (float_of_string (get_string ()), !line))
    and parse_comment stream =
      match Stream.peek stream with
	  Some('\010' | '\013') ->
	    Stream.junk stream;
	    incr line;
	    next_token stream
	| Some _ -> Stream.junk stream; parse_comment stream
	| _ -> raise Stream.Failure
    in
      Stream.from (fun count -> next_token input)
;;





(** Parse a path name from the input stream. *)

let rec parse_pathname input =
  Pathname (parse_pathname_identifier input [])
and parse_pathname_identifier input path =
  try
    match Stream.peek input with
	Some Id (name, _) ->
	  Stream.junk input;
	  parse_pathname_double_colon input (path@[name])
      | _ -> path
  with
      Eof -> path
and parse_pathname_double_colon input path =
  try
    match Stream.peek input with
	Some DoubleColon _ ->
	  Stream.junk input;
	  parse_pathname_identifier input path
      | _ -> path
  with
      Eof -> path





(** Parse an expression.

    This function is the main entry point to the parser if a constraint
    is to be parsed from the model. *)
let rec parse_expression stream : oclast =
  let token = Stream.peek stream
  in
    match token with
	_ -> assert false
;;


type oclconstraint = string * string option * oclast
;;





let rec parse_constraints input : oclconstraint list =
  (* We expect something of the form stereotype name?: expression
     here. *)
  match Stream.peek input with
      Some Keyword (( "inv" | "pre" | "post" | "init" | "derive" as st), _) ->
	Stream.junk input;
	begin
	  match Stream.peek input with
	      Some Colon _ ->
		(st, None, parse_expression input)::(parse_constraints input)
	    | Some Id (name, _) ->
		Stream.junk input;
		begin
		  match Stream.peek input with
		      Some Colon _ ->
			(st, Some name, parse_expression input) ::
			  (parse_constraints input)
		    | _ -> []
		end
	    | _ -> []
	end
    | _ -> []
;;




type oclcontext =
    string option * string option * oclast * ocltypespec option *
      (oclconstraint list)
;;




let parse_context input : oclcontext =
  match Stream.peek input with
      Some Keyword ("context", _) ->
	begin
	  match Stream.peek input with
	      Some Id (_, _) ->
		let name = parse_pathname input in
		  begin
		    match Stream.peek input with
			Some Colon _ ->
			  (None, None, name, None, [])
		      | Some LParen _ ->
			  (None, None, name, None, [])
		      | Some Keyword ("endpackage", _) ->
			  Stream.junk input; (None, None, name, None, [])
		      | _ ->
			  (None, None, name, None, parse_constraints input)
		  end
	    | _ -> assert false
	end
    | Some Keyword ("endpackage", _) -> Stream.junk input;
	(None, None, Error, None, [])
    | _ -> (None, None, Error, None, [])
;;





type oclpackage = oclast option * oclcontext list ;;





(** Parse a package declaration. *)

let rec parse_package input =
  (* We expect a package keyword. *)
    match Stream.peek input with
	Some Keyword ("package", _) ->
	  Stream.junk input; parse_package_name input
      | _ -> assert false
and parse_package_name input =
  let name = (parse_pathname input) in
    (Some name, (parse_package_context input))
and parse_package_context input =
  match Stream.peek input with
      Some Keyword ("context", _) ->
	(parse_context input) :: parse_package_context input
    | Some Keyword ("endpackage", _) -> Stream.junk input; []
    | _ -> raise ParseError






(** Parse a file from input.

    The result is a list of packages, where a package without
    a name is considered to be the top-level package. *)

let rec parse_file input: oclpackage list =
  try
    match Stream.peek input with
	Some Keyword ("package", _) ->
	  (parse_package input) :: (parse_file input)
      | Some Keyword ("context", _) ->
	  (None, [parse_context input]) :: (parse_file input)
      | _ -> raise ParseError
  with
      Eof -> []





(** Parse an expression from a string *)
let expression_from_string s =
  parse_expression (lexer (Stream.of_string s))
;;





(** Parse an expression from a file *)
let expression_from_file name =
  parse_expression (lexer (Stream.of_channel (open_in name)))
;;





(** Parse an OCL file from a string *)
let from_string s =
  parse_file (lexer (Stream.of_string s))
;;





(** Parse an OCL file from a file *)
let from_file name =
  parse_file (lexer (Stream.of_channel (open_in name)))
;;
