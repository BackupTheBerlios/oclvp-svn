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





type oclconstraint =
    { stereotype : string;
      constraintname : string option;
      expression : oclast }





type oclcontext = { self: string option;
		    xxx: string option;
		    context: oclast;
		    typespec: Type.t option;
		    constraints: oclconstraint list }

type oclpackage = { packagename: oclast option;
		    contextdecls: oclcontext list }

val parse_pathname : Lexer.token Stream.t -> oclast

val prettyprint_pathname : string list -> string

val from_string : string -> oclpackage list

val from_file : string -> oclpackage list

val expression_to_xml: XmlTextWriter.xmlwriter -> oclast -> unit

val unit_to_xml: XmlTextWriter.xmlwriter -> oclpackage list -> unit
