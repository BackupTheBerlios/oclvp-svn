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

(** Definition of the abstract syntax of class diagrams and their parts.

    @author Marcel Kyas <mkyas\@users.berlios.de>
    @since 0.0
    @version 0.0 *)

(** Definition of the abstract syntax of an attribute. *)
module Attribute: sig

  type init_t =
      (** The type of the initializer of an attribute. *)
      None
	(** No initializer has been given. *)
    | Opaque of string * string option * string
	(** The initializer has been given as an opaque string. the
	    field lang describes the language used to specify the
	    initializer, whereas value is a string representing the
	    value initializing the attribute. *)
    | OclExpression of string option * OCL.oclast
	(** The initializer has been provided in OCL, and this value contains
	    the abstract syntax tree of this string. *)

  type t
    (** The type of an attribute.  The type is abstract. *)

  val create: string -> string -> init_t -> t
    (** Create a new attribute.

	@param name  The name of the attribute.
	@param typespec  The type of the attribute.
	@param init  The initialization of the attribute.

	@return An attribute instance. *)
end





(** Defines the abstract syntax of an parameter. *)
module Parameter : sig

  (** The direction of a parameter. *)
    type dir_t =
	In
	(** The parameter is an input parameter, i.e., its value will
	    only be read in the method body, and never be written
	    to. *)
      | Out
	  (** The parameter is an output parameter, i.e., its value
	      will never be read in the method body, but it may be written
	      to. *)
      | InOut
	  (** The parameter is an input-/output parameter, i.e., its
	      value may be read from and written to in the method body. *)

    type t
      (** The type of a parameter.  Its type is abstract. *)

    val create: string -> dir_t -> string -> t
      (** Create a new parameter node.

	  @param name  The name of the parameter.
	  @param dir  The direction of the parameter.
	  @param string type The type of the parameter.

	  @return The new parameter node. *)

end





(** Defines the abstract syntax of an operation. *)
module Operation : sig
  type t

  val create: string -> Parameter.t list -> t
    (** Create a new Operation node. *)
end





(** Defines the abstract syntax of a classifier. *)
module Classifier : sig
  type t

  val create: string -> t

  val add_attribute: t -> Attribute.t -> unit
end
