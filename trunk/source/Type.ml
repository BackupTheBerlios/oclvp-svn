(*
 * ClassDiagram.ml -- Abstract syntax of a class diagram.
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

open XmlTextWriter
open Lexer

(** Implementation of the abstract syntax of types. *)
type t =
    (** The type of a type in UML and OCL. *)
    Untyped
      (** No type is defined for the element. *)
  | Any
      (** The type is not specific, i.e., it represents any type. *)
  | Void
      (** The type without any instances. *)
  | Simple of string
      (** The type is a classifier or data type which is not
	  parameterized. *)
  | Abstraction of string * string * t
      (** The type is a type abstraction. The first component
	  provides a name of the type, the second component provides
	  a type variable name, and the third component provides a
	  lower bound. *)
  | Application of t * t
      (** The type is a template instantiation. *)
  | Variable of string * t
      (** The type is a (universally quantified variable), where the
	  type component describes the lower bound. *)
  | Union of t list
      (** The union of many types. *)
  | Intersection of t list
      (** The intersection of many types. *)




let rec prettyprint_typespec t =
  match t with
      Simple n -> n
    | Application (n, t) -> "(" ^ (prettyprint_typespec n) ^ ")" ^
	"(" ^ (prettyprint_typespec t) ^ ")"
    | Variable (v, t) -> "$" ^ v ^ " <: " ^ (prettyprint_typespec t)
    | Union l -> (prettyprint_union " ++ " l)
    | Intersection l -> (prettyprint_union " ** " l)
    | _ -> assert false
and prettyprint_union oper l =
  match l with
      [] -> assert false
    | a::[] -> (prettyprint_typespec a)
    | t::r -> (prettyprint_typespec t) ^ oper ^ (prettyprint_union oper r)






let rec typespec_to_xml writer ty =
  match ty with
      Simple n ->
	start_element writer "type";
	write_attribute writer "name" n;
	end_element writer
    | Application (n, r) ->
	start_element writer "application";
	typespec_to_xml writer n;
	typespec_to_xml writer r;
	end_element writer
    | Variable (v, a) ->
	start_element writer "typevariable";
	write_attribute writer "name" v;
	end_element writer
    | Union l ->
	start_element writer "typeunion";
	typespec_list_to_xml writer l;
	end_element writer
    | Intersection l ->
	start_element writer "typeintersection";
	typespec_list_to_xml writer l;
	end_element writer
    | _ -> assert false
and typespec_list_to_xml writer l =
  match l with
      [t] -> typespec_to_xml writer t
    | t::r -> typespec_to_xml writer t; typespec_list_to_xml writer r
    | _ -> assert false





(** Parse a type specification. *)
let rec parse input =
  match Stream.peek input with
      Some Id (name, _) ->
	Stream.junk input;
	begin
	  match Stream.peek input with
	      Some LParen _ ->
		Stream.junk input;
		let arg = parse input in
		  begin
		    match Stream.peek input with
			Some RParen _ -> Application
			  (Simple name, arg)
		      | Some t -> raise  (BadToken ((get_token_name t),
						    (get_token_line t), ")"))
		      | None -> raise (Eof "in type specification")
		  end
	    | _ -> Simple name
	end
    | Some t -> raise (BadToken ((get_token_name t), (get_token_line t),
				 "<<identifier>>"))
    | None -> raise (Eof "in type specification")
