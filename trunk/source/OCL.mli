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

type
    oclast =
      Value of ObjectDiagram.value
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

type oclconstraint = string * string option * oclast

type oclcontext =
    string option * string option * oclast * ocltypespec option *
      (oclconstraint list)

type oclpackage = oclast option * oclcontext list

val prettyprint_typespec : ocltypespec -> string
(** Pretty print a type specification. *)

type ocltoken

val parse_pathname : ocltoken Stream.t -> oclast

val prettyprint_pathname : string list -> string

val lexer : char Stream.t -> ocltoken Stream.t

val from_file : string -> oclpackage list

