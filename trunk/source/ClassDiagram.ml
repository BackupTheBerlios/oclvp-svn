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

(** Implementation of the abstract syntax of attributes. *)
module Attribute =
  struct
    type init_t =
	None
      | Opaque of string * string option * string
      | OclExpression of string option * OCL.oclast

    type t = { name: string; typespec: string; init: init_t }

    let create n t i = {name = n; typespec = t; init = i; }

    let name a = a.name

    let typespec a = a.typespec

    let init a = a.init

  end





module Parameter =
  struct

    type dir_t =
	In
      | Out
      | InOut

    type t = {
      name: string;
      dir: dir_t;
      typespec: string;
    }

    let create n d t = { name = n; dir = d; typespec = t; }

  end

module Operation =
  struct

    type t = { name: string;
	       params: Parameter.t list; }

    let create n p = { name = n; params = p; }

  end

module Classifier =
  struct

    type t = {
      name: string;
      attributes: (string, Attribute.t) Hashtbl.t;
      operations: (string, string list) Hashtbl.t
    }

    let create n = {
      name = n;
      attributes = Hashtbl.create 7;
      operations = Hashtbl.create 7
    }

    let name c = c.name

    let add_attribute c a =
      try
	let t = Hashtbl.find c.attributes a.Attribute.name in
	  assert false
      with
	  Not_found -> Hashtbl.add c.attributes a.Attribute.name a
  end
