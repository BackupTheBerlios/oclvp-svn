(*
 * SUML.ml -- Read SUML files.
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

let print_node reader =
  print_endline
    ((XmlReader.name reader) ^ ": " ^
      (XmlReader.string_of_nodetype (XmlReader.node_type reader)) ^
	" at line " ^ (string_of_int (XmlReader.line_number reader)))





let read_constraint reader =
  (** Read a constraint from the file.

      <!ELEMENT constraint (#PCDATA)>
      <!ATTLIST constraint stereotype CDATA #REQUIRED
                           name CDATA #IMPLIED
                           lang CDATA #REQUIRED> *)
  assert ((XmlReader.name reader = "constraint") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
          ("constraint", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
        | (_, XmlReader.Text) -> continue := XmlReader.read reader
        | (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done





let read_action reader =
  (* Read an action.

     <!ELEMENT action (#PCDATA)>
     <!ATTLIST action lang  CDATA #REQUIRED> *)
  assert ((XmlReader.name reader = "action") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
        match (XmlReader.name reader, XmlReader.node_type reader) with
          | (_, XmlReader.Text) -> continue := XmlReader.read reader
          | (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
          | ("action", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
	  | _ -> continue := false (* Caller decides if this is an error. *)
    done





let read_guard reader =
  (* Read a guard.

     <!ELEMENT guard (#PCDATA)>
     <!ATTLIST guard lang CDATA #REQUIRED> *)
  assert ((XmlReader.name reader = "guard") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
          ("guard", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
        | (_, XmlReader.Text) -> continue := XmlReader.read reader
        | (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done





let read_trigger reader =
  (* Read a trigger.

     <!ELEMENT trigger EMPTY>
     <!ATTLIST trigger idref IDREF #REQUIRED> *)
  assert ((XmlReader.name reader = "trigger") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let trigger = () in
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
          (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
        | ("trigger", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
	| _ -> continue := false (* Caller decides if this is an error. *)
    done





let read_transition reader =
  (* Read a transition.

     <!ELEMENT transition (trigger*,guard?,action?)>
     <!ATTLIST transition name CDATA #IMPLIED
                          source IDREF #REQUIRED
                          target IDREF #REQUIRED> *)
  assert ((XmlReader.name reader = "transition") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
          ("trigger", XmlReader.StartElement) -> read_trigger reader
        | ("guard", XmlReader.StartElement) -> read_guard reader
        | ("action", XmlReader.StartElement) -> read_action reader
        | ("transition", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
        | (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done





let read_deferrable reader =
  (* Parse a deferrable.

     <!ELEMENT deferrable EMPTY>
     <!ATTLIST deferrable idref IDREF #REQUIRED> *)
  assert ((XmlReader.name reader = "deferrable") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match ((XmlReader.name reader), (XmlReader.node_type reader)) with
          ("deferrable", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
        | (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done





let rec read_region reader =
  (* Parse a region.

     <!ELEMENT region (state*,transition* )>
     <!ATTLIST region id ID #IMPLIED> *)
  assert ((XmlReader.name reader = "region") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
        match ((XmlReader.name reader), (XmlReader.node_type reader)) with
          ("region", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
        | ("state", XmlReader.StartElement) -> read_state reader
        | ("transition", XmlReader.StartElement) -> read_transition reader
        | (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done
and read_state reader =
  (* Parse a state.

     <!ELEMENT state (constraint*,deferrable*,region* )>
     <!ATTLIST state name CDATA #IMPLIED
                     id ID #REQUIRED
                     kind (normal|initial) "normal"> *)
  assert ((XmlReader.name reader = "state") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
          ("state", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
        | ("constraint", XmlReader.StartElement) -> read_constraint reader
        | ("deferrable", XmlReader.StartElement) -> read_deferrable reader
        | ("region", XmlReader.StartElement) -> read_region reader
        | (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done




let read_statemachine reader =
  (* Parse a state machine.

     <!ELEMENT statemachine (region+)>
     <!ATTLIST statemachine name CDATA #IMPLIED
                            id ID #IMPLIED> *)
  assert ((XmlReader.name reader = "statemachine") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
          ("statemachine", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
        | ("region", XmlReader.StartElement) -> read_region reader
        | (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done





let read_associationend reader =
  (* Parse an association end.

     <!ELEMENT associationend EMPTY>
     <!ATTLIST associationend name CDATA #IMPLIED
                              class CDATA #REQUIRED
                              aggregation (none|aggregate|composite) "none"
                              isnavigable (true|false) #IMPLIED> *)
  assert ((XmlReader.name reader = "associationend") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
          ("associationend", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
        | (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done




let read_association reader =
  (* Read an association.

     <!ELEMENT association (associationend,associationend)>
     <!ATTLIST association name CDATA #IMPLIED
                           export (yes|no) "yes"> *)
  assert ((XmlReader.name reader = "association") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
          ("associationend", XmlReader.StartElement) ->
	    read_associationend reader
        | ("association", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
        | (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_generalization reader =
  (* Read an generalization.

     <!ELEMENT generalization EMPTY>
     <!ATTLIST association parent ID #REQUIRED
                           child ID #REQUIRED
                           export (yes|no) "yes"> *)
  assert ((XmlReader.name reader = "generalization") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match ((XmlReader.name reader), (XmlReader.node_type reader)) with
          ("generalization", XmlReader.EndElement) ->
	    ignore (XmlReader.read reader); continue := false
        | (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_initializer reader =
  (** Read an implementation from the file.

      <!ELEMENT implementation (#PCDATA)>
      <!ATTLIST implementation lang CDATA #REQUIRED> *)
  assert ((XmlReader.name reader = "initializer") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
          (_, XmlReader.Text) -> continue := XmlReader.read reader
        | (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
        | ("initializer", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_implementation reader =
  (** Read an implementation from the file.

      <!ELEMENT implementation (#PCDATA)>
      <!ATTLIST implementation lang CDATA #REQUIRED> *)
  assert ((XmlReader.name reader = "implementation") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
          (_, XmlReader.Text) -> continue := XmlReader.read reader
        | (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
        | ("implementation", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_parameter reader =
  (* Read a parameter.

     <!ELEMENT parameter EMPTY>
     <!ATTLIST parameter name CDATA #REQUIRED
                         type CDATA #REQUIRED> *)
  assert ((XmlReader.name reader = "parameter") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
	  ("parameter", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
	| (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_reception reader model =
  assert ((XmlReader.name reader = "reception") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
          ("parameter", XmlReader.StartElement) ->
            read_parameter reader
        | ("constraint", XmlReader.StartElement) ->
            read_constraint reader
        | ("reception", XmlReader.EndElement) ->
              ignore (XmlReader.read reader); continue := false
        | (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
      done





let read_operation reader model =
  assert ((XmlReader.name reader = "operation") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue; do 
      match (XmlReader.name reader, XmlReader.node_type reader) with
	  ("parameter", XmlReader.StartElement) ->
            read_parameter reader
	| ("constraint", XmlReader.StartElement) ->
            read_constraint reader
	| ("implementation", XmlReader.StartElement) ->
            read_implementation reader
	| ("operation", XmlReader.EndElement) -> 
            ignore (XmlReader.read reader); continue := false
	| (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_attribute reader model =
  (* Read an attribute.

     <!ELEMENT attribute (initializer?)>
     <!ATTLIST attribute name CDATA #REQUIRED
                         type CDATA #REQUIRED
                         export (yes|no) "yes"> *)
  assert ((XmlReader.name reader = "attribute") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
	  ("initializer", XmlReader.StartElement) -> read_initializer reader
	| ("attribute", XmlReader.EndElement) ->
	    ignore (XmlReader.read reader); continue := false
	| (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_class reader model =
  assert ((XmlReader.name reader = "class") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
	  ("template", XmlReader.StartElement) ->
	    assert false
	| ("bind", XmlReader.StartElement) ->
	    assert false
	| ("extends", XmlReader.StartElement) ->
	    assert false
	| ("attribute", XmlReader.StartElement) ->
	    read_attribute reader model
	| ("operation", XmlReader.StartElement) ->
	    read_operation reader model
	| ("reception", XmlReader.StartElement) ->
	    read_reception reader model
	| ("associationend", XmlReader.StartElement) ->
	    read_associationend reader
	| ("constraint", XmlReader.StartElement) ->
	    read_constraint reader
	| ("statemachine", XmlReader.StartElement) ->
	    read_statemachine reader
	| ("class", XmlReader.EndElement) ->
	    ignore (XmlReader.read reader); continue := false
	| (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let rec read_package reader model =
  assert ((XmlReader.name reader = "package") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue  do
      match (XmlReader.name reader, XmlReader.node_type reader) with
	  ("package", XmlReader.StartElement) ->
	    read_package reader model
	| ("class", XmlReader.StartElement) ->
	    read_class reader model
	| ("association", XmlReader.StartElement) ->
	    read_association reader
	| ("generalization", XmlReader.StartElement) ->
	    read_generalization reader
	| ("package", XmlReader.EndElement) ->
	    ignore (XmlReader.read reader); continue := false
	| (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> print_node reader; exit 1
    done





let read_head reader model =
  assert ((XmlReader.name reader = "head") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
	  ("meta", XmlReader.StartElement) ->
            continue := XmlReader.read reader
	| ("meta", XmlReader.EndElement) -> continue := XmlReader.read reader
	| ("head", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
	| ("suml", XmlReader.EndElement) -> continue := false
	| (_, XmlReader.SigWhitespace) ->  continue := XmlReader.read reader
	| _ -> print_node reader; exit 1
    done





let read_model reader model =
  assert ((XmlReader.name reader = "suml") &&
    (XmlReader.node_type reader = XmlReader.StartElement));
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
	  ("head", XmlReader.StartElement) ->
            read_head reader model
	| ("package", XmlReader.StartElement) ->
            read_package reader model
	| ("class", XmlReader.StartElement) ->
            read_class reader model
	| ("association", XmlReader.StartElement) ->
            read_association reader
	| ("generalization", XmlReader.StartElement) ->
            read_generalization reader
	| ("suml", XmlReader.EndElement) ->
            ignore (XmlReader.read reader); continue := false
	| (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader
	| _ -> print_node reader; exit 1
    done





let from_file name =
  let model = Model.create name in
  let reader = XmlReader.from_filename name in
  let continue = ref (XmlReader.read reader) in
    while !continue do
      match (XmlReader.name reader, XmlReader.node_type reader) with
	  ("suml", XmlReader.StartElement) ->
              begin
                match XmlReader.get_attribute reader "version" with
                   "0.2" -> read_model reader model
                  | _ -> print_node reader; exit 1
              end;
              continue := false
	| ("suml", XmlReader.Doctype) -> continue := XmlReader.read reader
	| (_, XmlReader.SigWhitespace) -> continue := XmlReader.read reader 
	| _ -> print_node reader; exit 1
    done;
    model
