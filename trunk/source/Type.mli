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

(** Definition of the abstract syntax of types.

    @author Marcel Kyas <mkyas\@users.berlios.de>
    @since 0.0
    @version 0.0 *)

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

val prettyprint_typespec: t -> string

val typespec_to_xml: XmlTextWriter.xmlwriter -> t -> unit

val parse: Lexer.token Stream.t -> t
