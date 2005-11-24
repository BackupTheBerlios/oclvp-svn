(*
 * OCL.mli -- Definition of the abstract syntax of OCL.
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





(** The type of abstract syntax tree nodes of type expressions. *)
type
  ocltypespec =
    TypeError (** Indicates a type checking error *)
  | Name of string (** A simple type name, e.g. Integer, Boolean, ... *)
  | Application of string * ocltypespec (** The application of an ocltypespec
                            to the parameterized type described by string *)
  | Variable of string (** A type variable *)
  | Union of ocltypespec list (** A union type *)
  | Intersection of ocltypespec list (** An intersection type *)

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
    | Wildcard of ocltypespec option
    | Let of oclvardecl list * oclast
    | Self
    | Error
  and
    oclvardecl = { varname: string; typespec: ocltypespec option;
		   init: oclast option }





type oclconstraint =
    { stereotype : string;
      constraintname : string option;
      expression : oclast }





type oclcontext = { self: string option;
		    xxx: string option;
		    context: oclast;
		    typespec: ocltypespec option;
		    constraints: oclconstraint list }

type oclpackage = { packagename: oclast option;
		    contextdecls: oclcontext list }

val prettyprint_typespec : ocltypespec -> string
(** Pretty print a type specification. *)

type ocltoken

val parse_pathname : ocltoken Stream.t -> oclast

val prettyprint_pathname : string list -> string

val lexer : char Stream.t -> ocltoken Stream.t

val from_string : string -> oclpackage list

val from_file : string -> oclpackage list

val typespec_to_xml: XmlWriter.xmlwriter -> ocltypespec -> unit

val expression_to_xml: XmlWriter.xmlwriter -> oclast -> unit

val unit_to_xml: XmlWriter.xmlwriter -> oclpackage list -> unit
