(*
 * ObjectDiagram.ml -- Definition of the abstract syntax of OCL.
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

open ObjectDiagram;;

(** Define a type specification as they occur in our extended
    type checker  *)
type
  ocltypespec =
    TypeError (** Indicates a type checking error *)
  | Name of string (** A simple type name, e.g. Integer, Boolean, ... *)
  | Application of string * ocltypespec (** The application of an ocltypespec
                            to the parameterized type described by string *)
  | Variable of string (** A type variable *)
  | Union of ocltypespec list (** A union type *)
  | Intersection of ocltypespec list (** An intersection type *)
  ;;

(** Define the data type of the abstract syntax of OCL as used in the thesis.

    Contrary to the OCL meta model we do not use inheritance here, but
    a purely functional style. *)
type
    oclast =
      Value of value
    | Identifier of string
    | Collection of string * oclast list
    | If of oclast * oclast * oclast
    | AttributeCall of oclast * string
    | OperationCall of oclast * string * oclast list
    | CollectionCall of oclast * string * oclast list
    | Iterate of oclast * string * oclvardecl list * oclvardecl * oclast
    | Let of string * oclast list * oclast
    | Error
  and
    oclvardecl = { name: string; typespec: ocltypespec; init: oclast }
  ;;

(** The type checker

    For the case of allInstances, we have to make sure that the callee
    is really a type.  The abstract syntax assumes that the expression
    denoting the callee is really a general expression.  Therefore, we
    match on Variable for the callee argument, but we have to assert
    that the name of the variable coincides with the name of a
    class in the class diagram or a data type name.  *)
let rec ocltypechecker cd env expression =
  match expression with
    Value Null -> Name "OclAny"
  | Value Reference r -> TypeError
  | Value Boolean b -> Name "Boolean"
  | Value Integer i -> Name "Integer"
  | Value Real f -> Name "Real"
  | Value String s -> Name "String"
  | Value Bag b ->
      Application ("Bag", Union (List.map (ocltypechecker cd env)
				   (List.map (function x -> Value x) b)))
  | Value Sequence b ->
      Application ("Sequence", Union (List.map (ocltypechecker cd env)
					(List.map (function x -> Value x) b)))
  | Value Set b ->
      Application ("Set", Union (List.map (ocltypechecker cd env)
				   (List.map (function x -> Value x) b)))
  | Identifier i -> env i
  | Collection (n, e) ->
      Application (n, Union (List.map (ocltypechecker cd env) e))
  | If (c, e, f) ->
        (match ocltypechecker cd env c with
             Name "Boolean" -> Union [
                 ocltypechecker cd env e;
                 ocltypechecker cd env f]
           | _ -> TypeError)
  | AttributeCall (c, n) ->
      let ct = ocltypechecker cd env c
      in
      TypeError (* fetch () n *)
  | OperationCall (Identifier i, "allInstances", []) ->
        if true (* i is a type name *) then
            (Application ("Set", (Name i)))
        else
            TypeError
  | OperationCall (callee, "flatten", []) ->
      let calleetype = ocltypechecker cd env callee
      in ( match calleetype with
               Application (n1, Application(n2, t2)) -> Application (n1, t2)
             | t -> t )
  | OperationCall (callee, name, args) ->
        let calleetype = ocltypechecker cd env callee
        and argstype = List.map (ocltypechecker cd env) args
        in (match calleetype with
                TypeError -> TypeError
              | Application (n, a) ->
                  (* Here we have the map semantics. *)
                  Application (n, TypeError)
              | t -> TypeError )
                  (* Here we have the normal call semantics. *)
  | CollectionCall(c, n, a) -> TypeError
  | Iterate (c, n, d, a, e) ->
        let ct = ocltypechecker cd env c
        in ocltypechecker cd ((* XXX: update *) env) e
  | Error -> TypeError
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
let rec oclinterpreter cd cs ps env expr =
  match expr with
      Value v -> Value v
    | Identifier i -> env i
    | Collection (n, v) ->
	Collection (n, List.map (oclinterpreter cd cs ps env) v)
    | If (c, e, f) ->
	(match oclinterpreter cd cs ps env c with
             Value Boolean true -> oclinterpreter cd cs ps env e
           | Value Boolean false -> oclinterpreter cd cs ps env f
           | _ -> Error)
    | AttributeCall (c, n) ->
	( match oclinterpreter cd cs ps env c with
              Value Reference r ->
		Value (Hashtbl.find (List.nth cs.objects r).attributes n)
            | _ -> Error)
    | OperationCall (c, n, args) ->
	let vc = oclinterpreter cd cs ps env c
	and va = List.map (oclinterpreter cd cs ps env) args
	in (match vc with
		Value Boolean b -> Error
              | Value Integer i -> Error
              | Value Real r -> Error
              | Value String s -> Error
              | Collection (n, v) -> Error
              | _ -> Error)
    | CollectionCall (c, n, args) ->
	let va = List.map (oclinterpreter cd cs ps env) args
	in (match oclinterpreter cd cs ps env c with
		Collection ("Bag", values) -> Error
              | Collection ("Sequence", values) -> Error
              | Collection ("Set", values) -> Error
              | v -> oclinterpreter cd cs ps env
                  (CollectionCall (Collection ("Set", [v]), n, va)))
    | _ -> Error
;;
