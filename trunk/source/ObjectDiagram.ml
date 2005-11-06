(*
 * ObjectDiagram.ml -- Definition of an Object Diagram.
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

type value =
    Null
  | Reference of int
  | Boolean of bool
  | Integer of int
  | Real of float
  | String of string
  | Bag of value list
  | Sequence of value list
  | Set of value list
;;

type obj = {
  classifier: string;
  attributes: (string, value) Hashtbl.t
};;

let create_obj c = { classifier = c; attributes = Hashtbl.create 16 };;

(* The type of an object diagram is defined as a pair of objects and
   associations. *)
type objectdiagram = {
  nextobj: int;
  objects: (obj list);
  associations: (string, (int list)) Hashtbl.t
};;

let create_objectdiagram = {
  nextobj = 0;
  objects = [];
  associations = Hashtbl.create 16
};;

let create_object d c = {
  nextobj = d.nextobj + 1;
  objects = d.objects @ [create_obj c];
  associations = d.associations
};;
