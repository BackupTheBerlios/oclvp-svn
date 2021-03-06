(*
 * OCL.ml -- Definition of the abstract syntax of OCL.
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

open ObjectDiagram
open XmlTextWriter
open Lexer





(** Define the data type of the abstract syntax of OCL as used in my
    thesis.

    This abstract syntax is not derived from ptc/05-06-06.  It follows
    more standard ideas. *)
type oclast =
    | BooleanLiteral of bool
    | IntegerLiteral of int
    | StringLiteral of string
    | RealLiteral of float
    | Identifier of string
    | Pathname of string list
    | Collection of string * oclast list
    | Range of oclast * oclast
    | If of oclast * oclast * oclast
    | AttributeCall of oclast * string * bool
    | AssociationCall of oclast * string * oclast list * bool
    | OperationCall of oclast * string * bool * oclast list
    | CollectionCall of oclast * string * oclast list
    | Iterate of oclast * string * oclvardecl list * oclvardecl option * oclast
    | MessageExpr of oclast * string * oclast list
    | MessageSequenceExpr of oclast * string * oclast list
    | Wildcard of Type.t option
    | Let of oclvardecl list * oclast
    | Self
    | Error
  and
    oclvardecl = { varname: string; typespec: Type.t option;
		   init: oclast option }





let rec prettyprint_pathname path =
  match path with
      [] -> assert false
    | n::[] -> n
    | n::r -> n ^ "::" ^ (prettyprint_pathname r)




(** Print a tree. *)
let rec prettyprint tree =
  match tree with
      BooleanLiteral true -> "true"
    | BooleanLiteral false -> "false"
    | IntegerLiteral i -> string_of_int i
    | RealLiteral r -> string_of_float r
    | StringLiteral s -> "'" ^ s ^ "'"
    | Identifier i -> i
    | Pathname path -> (prettyprint_pathname path)
    | Collection (s, l) -> s ^ "{" ^ (prettyprint_args l) ^ "}"
    | Range (l, u) -> (prettyprint l) ^ ".." ^ (prettyprint u)
    | If (c, t, f) ->
	"if " ^ (prettyprint c) ^ " then " ^ (prettyprint t) ^ " else " ^
	  (prettyprint f) ^ " endif"
    | AttributeCall (s, a, p) -> (prettyprint s) ^ "." ^ a ^ (if p then "@pre" else "")
    | AssociationCall (_, _, _, _) -> assert false
    | OperationCall (s, m, p, a) ->
	(prettyprint s) ^ "." ^ m ^ (if p then "@pre" else "") ^ "(" ^
	  (prettyprint_args a) ^ ")"
    | CollectionCall (s, m, a) ->
	(prettyprint s) ^ "->" ^ m ^ "(" ^ (prettyprint_args a) ^ ")"
    | Iterate (c, n, v, ac, arg) ->
	(prettyprint c) ^ "->" ^ n
    | MessageExpr (r, n, a) ->
	(prettyprint r) ^ "^" ^ n ^ "(" ^ (prettyprint_args a) ^ ")"
    | MessageSequenceExpr (r, n, a) ->
	(prettyprint r) ^ "^^" ^ n ^ "(" ^ (prettyprint_args a) ^ ")"
    | Wildcard None -> "?"
    | Wildcard Some t -> "?: " ^ (Type.prettyprint_typespec t)
    | Let (v, i) -> "let " ^ (prettyprint_decls v) ^ " in " ^ (prettyprint i)
    | Self -> "self"
    | Error -> assert false
and prettyprint_args args =
  match args with
      [] -> ""
    | e::[] -> (prettyprint e)
    | e::r -> (prettyprint e) ^ ", " ^ (prettyprint_args r)
and prettyprint_decls l =
  match l with 
      [] -> assert false
    | d::[] -> (prettyprint_decl d)
    | d::r -> (prettyprint_decl d) ^ "," ^ (prettyprint_decls r)
and prettyprint_decl d =
  match d with
      { varname = name; typespec = None; init = None } ->
	name
    | { varname = name; typespec = None; init = Some i } ->
	name ^ " = " ^ (prettyprint i)
    | { varname = name; typespec = Some t; init = None } ->
	name ^ ": " ^ (Type.prettyprint_typespec t)
    | { varname = name; typespec = Some t; init = Some i } ->
	name ^ ": " ^ (Type.prettyprint_typespec t) ^ " = " ^ (prettyprint i)





(** Print a tree. *)
let rec expression_to_xml writer expr =
  match expr with
    | BooleanLiteral b ->
	start_element writer "booleanliteral";
	write_attribute writer "value" (string_of_bool b);
	end_element writer
    | IntegerLiteral i ->
	start_element writer "integerliteral";
	write_attribute writer "value" (string_of_int i);
	end_element writer
    | RealLiteral i ->
	start_element writer "realliteral";
	write_attribute writer "value" (string_of_float i);
	end_element writer
    | StringLiteral s ->
	start_element writer "realliteral";
	write_attribute writer "value" s;
	end_element writer
    | Identifier i ->
	start_element writer "identifier";
	write_attribute writer "name" i;
	end_element writer
    | Pathname path ->
	start_element writer "pathname";
	write_attribute writer "name" (prettyprint_pathname path);
	end_element writer
    | Collection (s, l) ->
	start_element writer "collectionliteral";
	write_attribute writer "type" s;
	List.iter (expression_to_xml writer) l;	
	end_element writer
    | Range (l, u) ->
	start_element writer "range";
	start_element writer "lower";
	expression_to_xml writer l;
	end_element writer;
	start_element writer "upper";
	expression_to_xml writer u;
	end_element writer;
	end_element writer
    | If (c, t, f) ->
	start_element writer "if";
	start_element writer "condition";
	expression_to_xml writer c;
	end_element writer;
	start_element writer "then";
	expression_to_xml writer t;
	end_element writer;
	start_element writer "else";
	expression_to_xml writer f;
	end_element writer;
	end_element writer
    | AttributeCall (s, a, p) ->
	start_element writer "attributecall";
	write_attribute writer "name" a;
	if p then write_attribute writer "ismarkedpre" "true";
	start_element writer "callee";
	expression_to_xml writer s;
	end_element writer;
	end_element writer;
    | AssociationCall (s, n, q, p) -> 
	start_element writer "associationcall";
	write_attribute writer "name" n;
	if p then write_attribute writer "ismarkedpre" "true";
	start_element writer "callee";
	expression_to_xml writer s;
	end_element writer;
	start_element writer "qualifiers";
	List.iter (fun e ->
		     start_element writer "qualifier";
		     expression_to_xml writer e;
		     end_element writer) q;
	end_element writer;
	end_element writer;
    | OperationCall (s, m, p, a) ->
	start_element writer "operationcall";
	write_attribute writer "name" m;
	if p then write_attribute writer "ismarkedpre" "true";
	start_element writer "callee";
	expression_to_xml writer s;
	end_element writer;
	start_element writer "arguments";
	List.iter (expression_to_xml writer) a;
	end_element writer;
	end_element writer;
    | CollectionCall (s, m, a) ->
	start_element writer "collectioncall";
	write_attribute writer "name" m;
	start_element writer "callee";
	expression_to_xml writer s;
	end_element writer;
	start_element writer "arguments";
	List.iter (expression_to_xml writer) a;
	end_element writer;
	end_element writer;
    | Iterate (c, n, v, ac, arg) ->
	start_element writer "iterate";
	write_attribute writer "name" n;
	start_element writer "callee";
	expression_to_xml writer c;
	end_element writer;
	end_element writer;
    | MessageExpr (r, n, a) -> assert false
    | MessageSequenceExpr (r, n, a) -> assert false
    | Wildcard None ->
	start_element writer "wildcard";
	end_element writer;
    | Wildcard Some t ->
	start_element writer "wildcard";
	Type.typespec_to_xml writer t;
	end_element writer;
    | Let (v, i) ->
	start_element writer "let";
	start_element writer "definitions";
	List.iter (vardecl_to_xml writer) v;
	end_element writer;
	start_element writer "in";
	expression_to_xml writer i;
	end_element writer;
	end_element writer;
    | Self -> write_element writer "self" None
    | Error -> assert false
and vardecl_to_xml writer decl =
  start_element writer "vardecl";
  write_attribute writer "name" decl.varname;
  begin
    match decl.init with
	Some init -> expression_to_xml writer init
      | _ -> ()
  end;
  end_element writer





(** The type checker

    For the case of allInstances, we have to make sure that the callee
    is really a type.  The abstract syntax assumes that the expression
    denoting the callee is really a general expression.  Therefore, we
    match on Variable for the callee argument, but we have to assert
    that the name of the variable coincides with the name of a
    class in the class diagram or a data type name.  *)
let rec ocltypechecker cd env (expression: oclast) =
  match expression with
    | BooleanLiteral b -> Type.Simple "Boolean"
    | IntegerLiteral i -> Type.Simple "Integer"
    | RealLiteral f -> Type.Simple "Real"
    | StringLiteral s -> Type.Simple "String"
    | Identifier i -> env i
    | Collection (n, e) ->
	Type.Application (Type.Simple n,
			  Type.Union (List.map (ocltypechecker cd env) e))
    | If (c, e, f) ->
        (match ocltypechecker cd env c with
             Type.Simple "Boolean" -> Type.Union [
               ocltypechecker cd env e;
               ocltypechecker cd env f]
           | _ -> Type.Untyped)
    | AttributeCall (c, n, _) ->
	let ct = ocltypechecker cd env c
	in
	  Type.Untyped (* fetch () n *)
    | OperationCall (Identifier i, "allInstances", _, []) ->
        if true (* i is a type name *) then
          (Type.Application (Type.Simple "Set", (Type.Simple i)))
        else
          Type.Untyped
    | OperationCall (callee, "flatten", _, []) ->
	let calleetype = ocltypechecker cd env callee
	in ( match calleetype with
		 Type.Application (n1, Type.Application(n2, t2)) ->
		   Type.Application (n1, t2)
               | t -> t )
    | OperationCall (callee, name, _, args) ->
        let calleetype = ocltypechecker cd env callee
        and argstype = List.map (ocltypechecker cd env) args
        in (match calleetype with
		Type.Application (n, a) ->
                  (* Here we have the map semantics. *)
                  Type.Application (n, Type.Untyped)
              | t -> Type.Untyped )
             (* Here we have the normal call semantics. *)
    | CollectionCall(c, n, a) -> Type.Untyped
    | Iterate (c, n, d, a, e) ->
        let ct = ocltypechecker cd env c
        in ocltypechecker cd ((* XXX: update *) env) e
    | Error -> Type.Untyped
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
let rec oclinterpreter cd cs ps env expr = ()





(*s Implementation of an OCL parser *)

(* The parser implemented here is a recursive descent parser.  Most of
   OCL is in LL(1), with some expressions being LL(2) parseable, and
   with iterate expressions requiring some generalised parsing
   mechanism.

   Building a table-driven parser for OCL is a difficult exercise;
   tools like yacc and ocamlyacc require major reworking of the
   grammar to obtain satisfying results.

   The tree returned by our parser is not necessarily correct.  The type
   checker will rewrite the abstract syntax tree generated by the parser
   and return a corrected and type-annotated syntax tree.  *)

(** Parse an identifier or operator name from the stream and return
    its name as an Identifier tree *)
let parse_identifier_or_operator input =
  match Stream.peek input with
      Some Id (id, _) -> Stream.junk input; id
    | Some Mult _ -> Stream.junk input; "*"
    | Some Plus _ -> Stream.junk input; "+"
    | Some Minus _ -> Stream.junk input; "-"
    | Some Div _ -> Stream.junk input; "/"
    | Some Less _ -> Stream.junk input; "<"
    | Some LessEq _ -> Stream.junk input; "<="
    | Some NotEq _ -> Stream.junk input; "<>"
    | Some Equals _ -> Stream.junk input; "="
    | Some Greater _ -> Stream.junk input; ">"
    | Some GreaterEq _ -> Stream.junk input; ">="
    | Some t -> raise (BadToken (get_token_name t, get_token_line t,
				 "<<identifier>>, *, +, -, /, <. <=, <>, =, >, >="))
    | None ->
	raise (Eof "exprecting <<identifier>>, *, +, -, /, <. <=, <>, =, >, >=")

(** Parse a path name from the input stream. *)
let rec parse_pathname input =
  Pathname (parse_pathname_identifier input [])
and parse_pathname_identifier input path =
  match Stream.peek input with
      Some Id (name, _) ->
	Stream.junk input;
	parse_pathname_double_colon input (path@[name])
    | _ -> path
and parse_pathname_double_colon input path =
  match Stream.peek input with
      Some DoubleColon _ ->
	Stream.junk input;
	parse_pathname_identifier input path
    | _ -> path




(** Parse an OCL expression.

    This function is the main entry point to the parser if a constraint
    is to be parsed from the model.

    The grammar parsed by this language is described in OMG:
    ptc/05-05-06.pdf, Chapter 9.  It does not implement the
    disambiguating rules.  I feel that it is more appropriate to leave
    this task to the type checker.  Meanwhile it is not obvious
    anymore what a valid expression is supposed to be, consequently,
    we try to use a good approximation and await bug reports which we
    the can try to evaluate.

    To do: Implement the \_ convention described in section 9.3,
    probably in the type checker. *)
let rec parse_expression input =
  match Stream.peek input with
      Some Keyword ("let", _) -> Stream.junk input; parse_let_expression input
    | Some _ -> parse_binary_expression input
    | None -> raise (Eof "while parsing expression")
and parse_let_expression input =
  (** Parse a let-expression.

      The parser is very liberal here, as it also allows let a in x. *)
  let l = parse_vardecls input in
    begin
      match Stream.peek input with
	  Some Keyword ("in", _) ->
	    Stream.junk input;
	    let i = parse_expression input in Let (l, i)
	| Some t -> raise (BadToken ((get_token_name t), (get_token_line t),
				     "in"))
	| None -> raise (Eof "in let expression")
    end
and parse_binary_expression input =
  (** Parse a binary expression, that is, a xor/iff expression.

      The following functions are concerned with OperationCallCS [A],
      but our implementation restricts to a sensible subset of expressions
      and respects the usual precedence rules.

      @parameter input The input stream.

      @returns An OperationCall tree representing the binary expression,
      or some other tree if the expression is not a binary expression. *)
  let lhs = parse_or_expression input in
    match Stream.peek input with
	Some Keyword ("xor", _) ->
	  Stream.junk input;
	  let rhs = parse_binary_expression input in
	    OperationCall (lhs, "xor", false, [rhs])
      | Some Keyword ("iff", _) ->
	  Stream.junk input;
	  let rhs = parse_binary_expression input in
	    OperationCall (lhs, "iff", false, [rhs])
      | _ -> lhs
and parse_or_expression input =
  let lhs = parse_and_expression input in
    match Stream.peek input with
	Some Keyword ("or", _) ->
	  Stream.junk input;
	  let rhs = parse_or_expression input in
	    OperationCall (lhs, "or", false, [rhs])
      | _ -> lhs
and parse_and_expression input =
  let lhs = parse_equals_expression input in
    match Stream.peek input with
	Some Keyword ("and", _) ->
	  Stream.junk input;
	  let rhs = parse_and_expression input in
	    OperationCall (lhs, "and", false, [rhs])
      | _ -> lhs
and parse_equals_expression input =
  let lhs = parse_relational_expression input in
    match Stream.peek input with
	Some NotEq _ ->
	  Stream.junk input;
	  let rhs = parse_relational_expression input in
	    OperationCall (lhs, "<>", false, [rhs])
      | Some Equals _ ->
	  Stream.junk input;
	  let rhs = parse_relational_expression input in
	    OperationCall (lhs, "=", false, [rhs])
      | _ -> lhs
and parse_relational_expression input =
  let lhs = parse_add_expression input in
    match Stream.peek input with
	Some Less _ ->
	  Stream.junk input;
	  let rhs = parse_add_expression input in
	    OperationCall (lhs, "<", false, [rhs])
      | Some LessEq _ ->
	  Stream.junk input;
	  let rhs = parse_add_expression input in
	    OperationCall (lhs, "<=", false, [rhs])
      | Some Greater _ ->
	  Stream.junk input;
	  let rhs = parse_add_expression input in
	    OperationCall (lhs, ">", false, [rhs])
      | Some GreaterEq _ ->
	  Stream.junk input;
	  let rhs = parse_add_expression input in
	    OperationCall (lhs, ">=", false, [rhs])
      | _ -> lhs
and parse_add_expression input =
  let lhs = parse_mult_expression input in
    match Stream.peek input with
	Some Minus _ ->
	  Stream.junk input;
	  let rhs = parse_add_expression input in
	    OperationCall (lhs, "-", false, [rhs])
      | Some Plus _ ->
	  Stream.junk input;
	  let rhs = parse_add_expression input in
	    OperationCall (lhs, "+", false, [rhs])
      | _ -> lhs
and parse_mult_expression input =
  let lhs = parse_unary_expression input in
    match Stream.peek input with
	Some Mult _ ->
	  Stream.junk input;
	  let rhs = parse_mult_expression input in
	    OperationCall (lhs, "*", false, [rhs])
      | Some Div _  ->
	  Stream.junk input;
	  let rhs = parse_mult_expression input in
	    OperationCall (lhs, "/", false, [rhs])
      | Some Keyword ("mod", _) ->
	  Stream.junk input;
	  let rhs = parse_mult_expression input in
	    OperationCall (lhs, "mod", false, [rhs])
      | _ -> lhs (* Not a binary operator, so end of the binary expression. *)
and parse_unary_expression input =
  (* Handle OperationCallCS [H]; for now we only allow - and not here instead
     of simple names. *)
  match Stream.peek input with
      Some Keyword ("not" as oper, _) ->
	Stream.junk input;
	OperationCall (parse_unary_expression input, oper, false, []);
    | Some Minus _ ->
	Stream.junk input;
	OperationCall (parse_unary_expression input, "-", false, []);
    | _ ->
	let expr = parse_atomic_expression input in
	  parse_postfix_expression expr input
and parse_postfix_expression expr input =
  (* Parse a postfix of an expression. *)
  match Stream.peek input with
      Some Dot _ ->
	Stream.junk input;
	begin
	  match Stream.npeek 4 input with
	      Id (name, _)::LParen _::_ -> (* OperationCallCS [C] *)
		Stream.junk input;
		Stream.junk input;
		let args = parse_arguments input in
		let res = OperationCall (expr, name, false, args) in
		  parse_postfix_expression res input
	    | Id (name, _)::LBracket _::_ ->
		(* AssociationCallCS[B] or AssociationClassCallCS[B]*)
		Stream.junk input;
		Stream.junk input;
		let args = parse_qualifiers input in
		let ismarkedpre = parse_pre_mark input in
		let res = AssociationCall (expr, name, args, ismarkedpre) in
		  parse_postfix_expression res input
	    | Id (name, _)::At _::Keyword ("pre", _)::LParen _::_ ->
		(* OperationCallCS [E] *)
		Stream.junk input;
		Stream.junk input;
		Stream.junk input;
		Stream.junk input;
		let args = parse_arguments input in
		let res = OperationCall (expr, name, true, args) in
		  parse_postfix_expression res input
	    | Id (name, _)::At _::Keyword ("pre", _)::_ ->
		(* AttributeCallCS [A] *)
		Stream.junk input;
		Stream.junk input;
		Stream.junk input;
		parse_postfix_expression (AttributeCall (expr, name, true))
		  input
	    | Id (name, _)::_ ->
		(* AttributeCallCS [A] *)
		Stream.junk input;
		parse_postfix_expression (AttributeCall (expr, name, false))
		  input
	    | t::_ -> raise (BadToken ((get_token_name t),
				       (get_token_line t), ")"))
	    | _ -> assert false
	end
    | Some Arrow _ ->
	Stream.junk input;
	begin
	  match Stream.peek input with
	      Some Id ("iterate", _) ->
		(* IterateExpressionCS ::=  *)
		Stream.junk input;
		begin 
		  match Stream.peek input with
		      Some LParen _ ->
			Stream.junk input;
			let res = parse_iterate_expression input expr in
			  parse_postfix_expression res input
		    | Some t -> raise (BadToken ((get_token_name t),
						 (get_token_line t), "("))
		    | _ -> assert false
		end
	    | Some Id (name, _) ->
		Stream.junk input;
		begin
		  match Stream.peek input with
		      Some LParen _ ->
			(* IteratorExpCS[A] ::= OclExpressionCS[1]
			   '->' simpleNameCS '(' (
			   VariableDeclarationCS,* '|') OclExpression
			   ')' The Cases [B-E] are \emph{not} parsed
			   here, because they conflict with the other
			   grammar rules; instead the type checker
			   will build the correct trees after
			   disambiguation.

			   OperationCallCS [B] ::= OclExpressionCS[1] '->'
			   simpleNameCS '(' args ')'. *)

			Stream.junk input;
			let res = parse_iterator_or_collectioncall input expr
			  name [] in parse_postfix_expression res input
		    | Some t -> raise (BadToken ((get_token_name t), (get_token_line t), "("))
		    | None -> raise (Eof "while parsing collection call expression")
		end
	    | Some t -> raise (BadToken ((get_token_name t),
					 (get_token_line t), "<<identifier>>"))
	    | None -> raise (Eof "while parsing collection call")
	end
    | Some Hat _ ->
	Stream.junk input;
	let name = parse_identifier_or_operator input in
	  begin
	    match Stream.peek input with
		Some LParen _ ->
		  Stream.junk input;
		  let args = parse_message_expr_args input in
		  let res = MessageExpr (expr, name, args) in
		    parse_postfix_expression res input
	      | Some t -> raise (BadToken ((get_token_name t),
					   (get_token_line t), "("))
	      | None -> raise (Eof "while parsing message expression")
	  end
    | Some HatHat _ ->
	Stream.junk input;
	let name = parse_identifier_or_operator input in
	  begin
	    match Stream.peek input with
		Some LParen _ ->
		  Stream.junk input;
		  let args = parse_message_expr_args input in
		  let res = MessageSequenceExpr (expr, name, args) in
		    parse_postfix_expression res input
	      | Some t -> raise (BadToken ((get_token_name t),
					   (get_token_line t), "("))
	      | None -> raise (Eof "while parsing message sequence expression")
	  end
    | _ -> expr
and parse_iterator_or_collectioncall input expr name (args: oclast list) =
  (* This function is used to parse an iterator expression or an operation
     call expression.  The grammar for this is ambigous and required the
     use of an GLR parser in the old prototype.

     Here we proceed as follows: Assume that after the lparen we have
     a list of variable declarations.  *)
  match Stream.npeek 2 input with
      [Id (_, _); (Colon _) | (Bar _) | (Semicolon _)] ->
	(* This is a type declaration or finishes a list of type
	   declarations, therefore we proceed with parsing iterator
	   expressions. Before we proceed we convert the list of
	   variables into a list of variable declarations without type
	   specifiers and initializers.

	   Observe that providing an initializer should not be allowed
	   in the variable declaration list of an iterate expression.
	   This would also introduce another ambiguity, because a term
	   of the form Id Equals Expression can also mean a boolean
	   predicate.  *)
	parse_iterator_expression input expr name
	  (List.map convert_to_vardecl args)
    | [Id (name, _); Comma _] ->
	(* This may be an iterator expression of an operation call expression,
	   so append a variable node and continue. *)
	Stream.junk input; Stream.junk input;
	parse_iterator_or_collectioncall input expr name
	  (args @ [Identifier name])
    | [Id (_, _); Equals _] ->
	(* Parse a variable declaration of the form variable =
	   initializer, which can also be a normal expression.  It is
	   actually nonsensical to parse this here, because the
	   semantics of these variable declarations is not defined for
	   iterate expressions.  On the other hand, the OCL grammar
	   allows it.  *)
	let arg = parse_expression input in
	  begin
	    match Stream.peek input with
		Some Comma _ -> Stream.junk input
	      | _ -> ()
	  end;
	  parse_iterator_or_collectioncall input expr name (args @ [arg])	  
    | _ -> 
	(* The list ends with an identifier; however, we conclude that
	   these are arguments to a call expression. It \emph{may} be
	   the case that this is an Iterator expression (as defined by
	   cases [B-E]) but the type checker will take care of this.

	   Since we do not know what kind of expression this is, we
	   assume that this is an ordinary operation call
	   expression. *)
	parse_collectioncall_expression input expr name args
and parse_iterator_expression input expr name decls =
  match Stream.peek input with
      Some Id (_, _) ->
	parse_iterator_expression input expr name
	  (decls @ [parse_vardecl input])
    | Some Semicolon _ ->
	assert false
    | Some Bar _ ->
	Stream.junk input;
	let arg = parse_expression input in 
	  begin
	    match Stream.peek input with
		Some RParen _ ->
		  Stream.junk input;
		  Iterate (expr, name, decls, None, arg)
	      | Some t -> raise (BadToken ((get_token_name t),
					   (get_token_line t), ")"))
	      | None -> raise (Eof "in iterator expression")
	  end
    | Some t -> raise (BadToken ((get_token_name t), (get_token_line t),
				 "<<identifier>> or ; or |"))
    | None -> raise (Eof "in iterator expression")
and parse_collectioncall_expression input expr name (args: oclast list) =
  let remainder = parse_arguments input in
    CollectionCall (expr, name, args @ remainder)
and convert_to_vardecl expr =
  match expr with
      Identifier name -> { varname = name; typespec = None; init = None }
    | OperationCall (Identifier name, "=", _, [arg]) ->
	{ varname = name; typespec = None; init = Some arg }
    | _ -> assert false
and parse_iterate_expression input expr =
  (* Parse an iterate expression *)
  let vardecls = parse_vardecls input in
    match Stream.peek input with
	Some Semicolon _ ->
	  Stream.junk input;
	  let decl = parse_vardecl input in
	    begin
	      match Stream.peek input with
		  Some Bar _ ->
		    Stream.junk input;
		    let arg = parse_expression input in
		      begin
			match Stream.peek input with
			    Some RParen _ ->
			      Stream.junk input;
			      Iterate (expr, "iterate", vardecls, Some decl, arg)
			  | Some t -> raise (BadToken ((get_token_name t),
						       (get_token_line t), ")"))
			  | None -> raise (Eof "in iterate expression")
		      end
		| Some t -> raise (BadToken ((get_token_name t),
					     (get_token_line t),
					     "|"))
		| None -> raise (Eof "in iterate expression")
	    end
      | Some Bar line ->
	  Stream.junk input;
	  begin
	    match vardecls with
		[decl] ->
		  let arg = parse_expression input in
		    begin
		      match Stream.peek input with
			  Some RParen _ ->
			    Stream.junk input;
			    Iterate (expr, "iterate", [], Some decl, arg)
			| Some t -> raise (BadToken ((get_token_name t),
						     (get_token_line t), ")"))
			| None -> raise (Eof "in iterate expression")
		    end
	      | _ -> raise (BadToken ("|", line, ";"))
	  end
      | Some t -> raise (BadToken ((get_token_name t), (get_token_line t),
				   "; or |"))
      | None -> raise (Eof "in iterate expression")
and parse_atomic_expression input =
  (* Parse atomic expressions. Here we need three tokens of lookahead,
     in order to recognize attribute calls marked pre.

     Note that two tokens would be sufficient, since we only need to see
     the @. *)
  match Stream.npeek 3 input with
      (Keyword ("true", _))::_ -> Stream.junk input; BooleanLiteral true
    | (Keyword ("false", _))::_ -> Stream.junk input; BooleanLiteral false
    | (Int (v, _))::_ -> Stream.junk input; IntegerLiteral v
    | (Float (v, _))::_ -> Stream.junk input; RealLiteral v
    | (Str (v, _))::_ -> Stream.junk input; StringLiteral v
    | [Id (name, _); At _; Keyword ("pre", _)] ->
	(* AttributeCallCS [B]: SimpleNameCS "@pre"
	   or OperationCallCS [F] *)
	Stream.junk input;
	Stream.junk input;
	Stream.junk input;
	begin
	  match Stream.peek input with
	      Some LParen _ -> (* OperationCallCS [F] *)
		Stream.junk input;
		let args = parse_arguments input;
		in OperationCall (Self, name, true, args)
	    | _ -> (* AttributeCallCS [B] *) AttributeCall (Self, name, true) 
	end
    | [Id (name, _); LParen _; _] ->
      	(* OperationCallCS [D]: SimpleNameCS "(" args ")" *)
	Stream.junk input;
	Stream.junk input;
	let args = parse_arguments input in
	  OperationCall(Self, name, false, args)
    | [Id (name, _); LBracket _; _] ->
	(* AssociationEndCallCS[B] or AssociationClassCallCS[B].

	   Disambiguation will be performed by the type checker. *)

	Stream.junk input; Stream.junk input;
	let args = parse_qualifiers input in
	let ismarkedpre = parse_pre_mark input in
	  AssociationCall(Self, name, args, ismarkedpre)
    | [Id ("Bag" | "OrderedSet" | "Sequence" | "Set" as n, _); LBrace _; _] ->
	(* CollectionLiteralCS *)
	Stream.junk input;
	Stream.junk input;
	parse_collection_literal n input
    | [Id ("Tuple" as name, _); LBrace _; _] ->
	(* TupleLiteralCS *)
	assert false;
    | [Id (_,_); DoubleColon _; _] ->
	let name = parse_pathname input in
	  begin
	    match Stream.peek input with
		Some LParen _ -> (* OperationCallCS [G] *)
		  assert false
	      | _ -> name
	  end
    | (Id (name, _))::_ -> Stream.junk input; Identifier name
    | (LParen _)::_ ->
	Stream.junk input;
	let e = parse_expression input in
	  begin
	    match Stream.peek input with
		Some RParen _ -> Stream.junk input; e
	      | Some t -> raise (BadToken ((get_token_name t), (get_token_line t), ")"))
	      | None -> raise (Eof "while parsing arguments")
	  end
    | (Keyword ("if", _))::_ -> parse_if_expression input
    | t::_ -> raise (BadToken ((get_token_name t), (get_token_line t),
			       "true, false, <<number>>, <<string>>, <<identifier>>, (, if"))
    | [] -> raise (Eof "while parsing arguments")
and parse_collection_literal name input =
  (** Parse a collection literal.

      The callee is assumet to have already consumed the name and the
      left brace.  The function mainly handles the case of empty
      collections. *)
  match Stream.peek input with
      Some RBrace _ -> Stream.junk input; Collection (name, [])
    | _ -> Collection (name, parse_collection_literal_parts input)
and parse_collection_literal_parts input =
  (** Parse the parts of a collection literal.

      Assumes, that the opening brace has already been consumed by the
      caller. *)
  let e = parse_collection_literal_part input in
    match Stream.peek input with
	Some RBrace _ -> Stream.junk input; []
      | Some Comma _ -> Stream.junk input;
	  e :: parse_collection_literal_parts input
      | Some t ->
	  raise (BadToken ((get_token_name t), (get_token_line t), ", or }"))
      | None -> raise (Eof "while parsing collection literal")
and parse_collection_literal_part input =
  (** Parse the part of a collection literal.  This is either an expression
      or a range expression. *)
  let part = parse_expression input in
    match Stream.peek input with
	Some DotDot _ ->
	  Stream.junk input; Range (part, parse_expression input)
      | _ -> part
and parse_if_expression input =
  match Stream.peek input with
      Some Keyword ("if", _) ->
	Stream.junk input;
	let c = parse_expression input in
	  begin
	    match Stream.peek input with
		Some Keyword ("then", _) ->
		  Stream.junk input;
		  let t = parse_expression input in
		    begin
		      match Stream.peek input with
			  Some Keyword ("else", _) ->
			    Stream.junk input;
			    let f = parse_expression input in
			      begin
				match Stream.peek input with
				    Some Keyword ("endif", _) ->
				      Stream.junk input;
				      If (c, t, f)
				  | Some t -> raise (BadToken ((get_token_name t), (get_token_line t), "endif"))
				  | None -> raise (Eof "while parsing else clause")
			      end
			| Some t -> raise (BadToken ((get_token_name t),
						     (get_token_line t),
						     "else"))
			| None -> raise (Eof "while parsing then clause")
		    end
	      | Some t -> raise (BadToken ((get_token_name t),
					   (get_token_line t), "then"))
	      | None -> raise (Eof "while parsing if-condition")
	  end
    | _ -> assert false
and parse_arguments input =
  (* Assume that the caller has consumed the opening parenthesis *)
  match Stream.peek input with
      Some RParen _ -> Stream.junk input; []
    | _ ->
	let args = parse_expression_list input in
	  begin
	    match Stream.peek input with
		Some RParen _ -> Stream.junk input; args
	      | Some t -> raise (BadToken ((get_token_name t),
					   (get_token_line t), ") or ,"))
	      | None -> raise (Eof "while parsing arguments")
	  end
and parse_message_expr_args input =
  match Stream.peek input with
      Some RParen _ ->
	Stream.junk input;
	[]
    | Some Question _ ->
	Stream.junk input;
	begin
	  match Stream.peek input with
	      Some Colon _ ->
		Stream.junk input;
		let ts = Type.parse input in
		  begin
		    match Stream.peek input with
			Some Comma _ ->
			  Stream.junk input;
			  (Wildcard (Some ts))::(parse_message_expr_args input)
		      | Some RParen _ ->
			  Stream.junk input;
			  [Wildcard (Some ts)]
		      | Some t -> raise (BadToken ((get_token_name t),
						   (get_token_line t),
						   ") or ,"))
		      | None -> raise (Eof "while pasing arguments of message or message sequence expression")
		  end
	    | Some Comma _ ->
		Stream.junk input;
		(Wildcard None)::(parse_message_expr_args input)
	    | Some RParen _ ->
		Stream.junk input;
		[Wildcard None]
	    | Some t -> raise (BadToken ((get_token_name t),
					 (get_token_line t), ": or , or )"))
	    | None -> raise (Eof "while pasing arguments of message or message sequence expression")
	end
    | Some _ ->
	let expr = parse_expression input in
	  begin
	    match Stream.peek input with
		Some Comma _ ->
		  Stream.junk input;
		  expr::(parse_message_expr_args input)
	      | Some RParen _ ->
		  Stream.junk input;
		  [expr]
	      | Some t -> raise (BadToken ((get_token_name t),
					   (get_token_line t), ") or ,"))
	      | None -> raise (Eof "while pasing arguments of message or message sequence expression")
	  end
    | None -> raise (Eof "while pasing arguments of message or message sequence expression")
and parse_qualifiers input =
  (* Assume that the caller has consumed the opening parenthesis *)
  match Stream.peek input with
      Some RBracket _ -> Stream.junk input; []
    | _ ->
	let args = parse_expression_list input in
	  begin
	    match Stream.peek input with
		Some RBracket _ -> Stream.junk input; args
	      | Some t -> raise (BadToken ((get_token_name t),
					   (get_token_line t), "] or ,"))
	      | None -> raise (Eof "while parsing qualifiers")
	  end
and parse_expression_list input =
  let expr = (parse_expression input) in
    match Stream.peek input with
	Some Comma _ ->
          Stream.junk input;
          expr:: (parse_expression_list input)
      | _ -> [expr]
and parse_pre_mark input =
  match Stream.peek input with
      Some At _ ->
	Stream.junk input;
	begin
	  match Stream.peek input with
	      Some Keyword ("pre", _) -> Stream.junk input; true
	    | Some t -> raise (BadToken ((get_token_name t),
					 (get_token_line t), "pre"))
	    | _ -> assert false
	end
    | _ -> false
and parse_vardecls input =
  match Stream.peek input with
      Some Id (_, _) ->
	let decl = parse_vardecl input in
	  begin
	    match Stream.peek input with
		Some Comma _ ->
		  Stream.junk input;
		  let rest = parse_vardecls input in
		    decl::rest
	      | _ -> [decl]
	  end
    | _ -> []
and parse_vardecl input =
  match Stream.peek input with
      Some Id (name, _) ->
	Stream.junk input;
	begin
	  match Stream.peek input with
	      Some Colon _ ->
		Stream.junk input;
		let t = Type.parse input in
		  begin
		    match Stream.peek input with
			Some Equals _ ->
			  Stream.junk input;
			  let i = parse_expression input in
			    { varname = name; typespec = None; init = Some i }
		      | _ -> { varname = name; typespec = Some t; init = None }
		  end
	    | Some Equals _ ->
		Stream.junk input;
		let i = parse_expression input in
		  { varname = name; typespec = None; init = Some i }
	    | _ -> { varname = name; typespec = None; init = None }
	end
    | Some t -> raise (BadToken ((get_token_name t), (get_token_line t),
				 "<<identifier>>"))
    | None -> raise (Eof "while parsing variable declaration")





(** A constraint. *)
type oclconstraint =
    { stereotype : string;
      constraintname : string option;
      expression : oclast }






(** Parse a list of constraints

    Here we expect a list of the form [stereotype name?: expression]. *)

let rec parse_constraints input =
  match Stream.peek input with
      Some Keyword (( "inv" | "pre" | "post" | "init" | "derive" as s), _) ->
	Stream.junk input;
	let n = parse_constraint_name input in
	  begin
	    match Stream.peek input with
		Some Keyword ("endpackage", _) | None ->
		  (* endpackage will be consumed in
		     parse\_package\_contexts! *)
		  [{ stereotype = s; constraintname = n;
		     expression = BooleanLiteral true }]
	      | Some Keyword (( "inv" | "pre" | "post" | "init" |
				    "derive" | "body"), _) ->
		  { stereotype = s; constraintname = n;
		    expression = BooleanLiteral true } ::
		    parse_constraints input
	      | Some _ ->
		  let e = parse_expression input in
		    { stereotype = s; constraintname = n; expression = e } ::
		      parse_constraints input
	  end
    | Some _ -> []
    | None -> []
and parse_constraint_name input =
  match Stream.peek input with
      Some Id (n, _) -> Stream.junk input; parse_constraint_colon input; Some n
    | Some Colon _ -> parse_constraint_colon input; None
    | Some t -> raise (BadToken ((get_token_name t), (get_token_line t), "<<id>>, :"))
    | None -> assert false
and parse_constraint_colon input =
  match Stream.peek input with
      Some Colon _ -> Stream.junk input
    | Some t -> raise (BadToken ((get_token_name t), (get_token_line t), ":"))
    | None -> assert false





type oclcontext = { self: string option;
		    xxx: string option;
		    context: oclast;
		    typespec: Type.t option;
		    constraints: oclconstraint list }





let parse_context input =
  match Stream.peek input with
      Some Keyword ("context", _) ->
        Stream.junk input;
	begin
	  match Stream.peek input with
	      Some Id (_, _) ->
		let name = parse_pathname input in
		  begin
		    match Stream.peek input with
			Some Colon _ ->
			  { self = None; xxx = None; context = name;
			    typespec = None; constraints = [] }
		      | Some LParen _ ->
			  { self = None; xxx = None; context = name;
			    typespec = None; constraints = [] }
		      | Some Keyword ("endpackage", _) ->
			  Stream.junk input; 
			  { self = None; xxx = None; context = name;
			    typespec = None; constraints = [] }
		      | Some _ ->
			  { self = None; xxx = None; context = name;
			    typespec = None;
			    constraints = parse_constraints input }
		      | None ->
			  { self = None; xxx = None; context = name;
			    typespec = None;
			    constraints = [] }
		  end
	    | _ -> assert false
	end
    | Some Keyword ("endpackage", _) ->
	Stream.junk input;
	{ self = None; xxx = None; context = Error; typespec = None;
	  constraints = [] }
    | _ -> 
	{ self = None; xxx = None; context = Error; typespec = None;
	  constraints = [] }
and parse_context_name input =
  match Stream.peek input with
      Some Id (_, line) ->
        let name = parse_pathname input in
        let constraints = parse_constraints input in
          (None, None, name, None, constraints)
    | Some t -> raise (BadToken ((get_token_name t), (get_token_line t), "<<id>>"))
    | None -> assert false





type oclpackage = { packagename: oclast option;
		    contextdecls: oclcontext list }





(** Parse a package declaration. *)

let rec parse_package input =
  (* We expect a package keyword. *)
    match Stream.peek input with
	Some Keyword ("package", _) ->
	  Stream.junk input;
	  parse_package_name input
      | _ -> assert false
and parse_package_name input =
  let name = (parse_pathname input) in
    { packagename = Some name; contextdecls = parse_package_contexts input }
and parse_package_contexts input =
  match Stream.peek input with
      Some Keyword ("context", _) ->
	let context = parse_context input in
	context :: parse_package_contexts input
    | Some Keyword ("endpackage", _) -> Stream.junk input; []
    | Some t -> raise (BadToken ((get_token_name t), (get_token_line t),
				 "context, endpackage"))
    | None -> assert false






(** Parse a file from input.

    The result is a list of packages, where a package without
    a name is considered to be the top-level package. *)

let rec parse_file input: oclpackage list =
  match Stream.peek input with
      Some Keyword ("package", _) ->
        let package = parse_package input in
	  package :: (parse_file input)
    | Some Keyword ("context", _) ->
        let context = parse_context input in
	  { packagename = None; contextdecls = [context] } ::
	    (parse_file input)
    | Some t -> raise (BadToken ((get_token_name t), (get_token_line t),
				 "context, package"))
    | None -> []





(** Parse an expression from a string *)
let expression_from_string s = parse_expression (lexer (Stream.of_string s))





(** Parse an expression from a file *)
let expression_from_file name =
  parse_expression (lexer (Stream.of_channel (open_in name)))





(** Parse an OCL file from a string *)
let from_string s = parse_file (lexer (Stream.of_string s))





(** Parse an OCL file from a file *)
let from_file name = parse_file (lexer (Stream.of_channel (open_in name)))





let constraint_to_xml writer con =
  start_element writer "constraint";
  write_attribute writer "stereotype" con.stereotype;
  begin
    match con.constraintname with
	None -> ()
      | Some name -> write_attribute writer "name" name
  end;
  expression_to_xml writer con.expression;
  end_element writer





let contextdecl_to_xml writer context =
  start_element writer "context";
  begin
    match context.context with
	Pathname n -> write_attribute writer "name" (prettyprint_pathname n)
      | _ -> assert false
  end;
  List.iter (constraint_to_xml writer) context.constraints;
  end_element writer





let package_to_xml writer package =
    match package with
	{ packagename = None; contextdecls = l } ->
	  List.iter (contextdecl_to_xml writer) l;
      | { packagename = Some Pathname name; contextdecls = l } ->
	  start_element writer "package";
	  write_attribute writer "name" (prettyprint_pathname name);
	  List.iter (contextdecl_to_xml writer) l;
	  end_element writer;
      | _ -> assert false





(** Write the contents of a compilation unit to XML. *)
let unit_to_xml writer packages =
  start_document writer None None None;
  start_element writer "oclvp";
  write_attribute writer "version" Version.oclvp_version;
  write_attribute writer "kind" "constraint unit";
  List.iter (package_to_xml writer) packages;
  end_document writer
