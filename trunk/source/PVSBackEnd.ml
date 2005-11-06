(*
 * PVSBackEnd.ml -- Write a tree to PVS.
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

(** These functions are used to convert a UML model to PVS. *)

open ObjectDiagram;;
open OCL;;

(** Mangle a name.  Some valid names of UML are reserved in PVS or they are
    not valid identifiers in PVS.  This function converts a name into a
    form which PVS accepts as input. *)
let pvs_mangle_name name =
  "name_" ^ name
;;


(** Convert an OCL expression to PVS. *)
let rec ocl_expr_to_pvs expr =
  match expr with
      Value Null -> "null"
    | Value Reference r -> "reference(" ^ (string_of_int r) ^ ")"
    | Value Boolean b -> if b then "true" else "false"
    | Value Integer i -> (string_of_int i)
    | Value Real f -> (string_of_float f)
    | Value String s -> "\"" ^ s ^ "\""
    | Value Bag b ->
	(match b with
	     [] -> "nil"
	   | [elt] -> (ocl_expr_to_pvs (Value elt))
	   | hd::tl->
	       "including(" ^ (ocl_expr_to_pvs (Value hd)) ^ ", " ^
		 (ocl_expr_to_pvs (Value (Bag tl))) ^ ")")
    | Value Sequence b ->
	(match b with
	     [] -> "nil"
	   | [elt] -> (ocl_expr_to_pvs (Value elt))
	   | hd::tl->
	       "cons(" ^ (ocl_expr_to_pvs (Value hd)) ^ ", " ^
		 (ocl_expr_to_pvs (Value (Sequence tl))) ^ ")")
    | Value Set b ->
	(match b with
	     [] -> "emptyset"
	   | [elt] -> "{" ^ (ocl_expr_to_pvs (Value elt)) ^ "}"
	   | hd::tl->
	       "union(" ^ (ocl_expr_to_pvs (Value hd)) ^ ", " ^
		 (ocl_expr_to_pvs (Value (Set tl))) ^ ")")
    | _ ->
	assert false
;;


(** Convert an action to PVS *)
let rec action_to_pvs action =
  "nil"
;;


(** Write a trainsition to PVS. *)
(*
let sm_trans_to_pvs t =
  print_string "(#";
  print_string ("source := " ^ (pvs_mangle_name t.src) ^ ",");
  print_string ("target := " ^ (pvs_mangle_name t.tgt) ^ ",");
  print_string ("trigger := " ^ (pvs_mangle_name t.trigger) ^ ",");
  print_string ("guard := (LAMBDA state: " ^ (ocl_expr_to_pvs t.guard) ^ ",");
  print_string ("action := (LAMBDA state: " ^ (action_to_pvs t.guard) ^ ",");
;;
*)
