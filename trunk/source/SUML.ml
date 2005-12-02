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

(** Parse an XML document in the SUML format and return a model. *)

(** Print a node. Used for debugging. *)
let print_node reader =
  print_endline
    ((XmlTextReader.name reader) ^ ": " ^
       (XmlTextReader.string_of_nodetype (XmlTextReader.node_type reader)) ^
       " at line " ^ (string_of_int (XmlTextReader.line_number reader)))




(** Obtain the export attribute of the current position of the reader
    and convert it to a boolean. The implied default value has to be
    supplied as a string. *)
let attribute_export reader default =
  let export = XmlTextReader.get_attribute_opt reader "export" default in
    if export = "true" then true else false




let read_constraint reader =
  (* Read a constraint from the file.

     <!ELEMENT constraint (#PCDATA)>
     <!ATTLIST constraint stereotype CDATA #REQUIRED
     name CDATA #IMPLIED
     lang CDATA #REQUIRED> *)
  assert ((XmlTextReader.name reader = "constraint") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let stereotype = XmlTextReader.get_attribute reader "stereotype" in
  let name = XmlTextReader.get_attribute_opt reader "name" "" in
  let lang = XmlTextReader.get_attribute reader "lang" in
  let continue = ref (XmlTextReader.read reader) in
  let result = ref "" in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
          ("constraint", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
        | (_, XmlTextReader.Text) ->
	    result := !result ^ (XmlTextReader.value reader);
	    continue := XmlTextReader.read reader
        | (_, XmlTextReader.SigWhitespace) ->
	    result := !result ^ (XmlTextReader.value reader);
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done;
    (* After having read the constraint we see whether we want to
       parse it or store it as an opaque constraint. *)
    begin
      match lang with
	  "OCL" -> ()
	| _ -> ()
    end




let read_action reader =
  (* Read an action.

     <!ELEMENT action (#PCDATA)>
     <!ATTLIST action lang  CDATA #REQUIRED> *)
  assert ((XmlTextReader.name reader = "action") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let action = ref "" in
  let lang = XmlTextReader.get_attribute reader "lang" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
        | (_, XmlTextReader.Text) ->
	    action := !action ^ (XmlTextReader.value reader);
	    continue := XmlTextReader.read reader
        | (_, XmlTextReader.SigWhitespace) ->
	    action := !action ^ (XmlTextReader.value reader);
	    continue := XmlTextReader.read reader
        | ("action", XmlTextReader.EndElement) ->
	    ignore (XmlTextReader.read reader); continue := false
	| _ -> continue := false (* Caller decides if this is an error. *)
    done





let read_guard reader =
  (* Read a guard.

     <!ELEMENT guard (#PCDATA)>
     <!ATTLIST guard lang CDATA #REQUIRED> *)
  assert ((XmlTextReader.name reader = "guard") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let lang = XmlTextReader.get_attribute reader "lang" in
  let guard = ref "" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
          ("guard", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
        | (_, XmlTextReader.Text) ->
	    guard := !guard ^ (XmlTextReader.value reader);
	    continue := XmlTextReader.read reader
        | (_, XmlTextReader.SigWhitespace) ->
	    guard := !guard ^ (XmlTextReader.value reader);
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done





let read_trigger reader =
  (* Read a trigger.

     <!ELEMENT trigger EMPTY>
     <!ATTLIST trigger idref IDREF #REQUIRED> *)
  assert ((XmlTextReader.name reader = "trigger") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let trigger = XmlTextReader.get_attribute reader "idref" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
          (_, XmlTextReader.SigWhitespace) -> 
	    continue := XmlTextReader.read reader
        | ("trigger", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
	| _ -> continue := false (* Caller decides if this is an error. *)
    done





let read_transition reader =
  (* Read a transition.

     <!ELEMENT transition (trigger*,guard?,action?)>
     <!ATTLIST transition name CDATA #IMPLIED
     source IDREF #REQUIRED
     target IDREF #REQUIRED> *)
  assert ((XmlTextReader.name reader = "transition") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let name = XmlTextReader.get_attribute_opt reader "source" "" in
  let source = XmlTextReader.get_attribute reader "source" in
  let target = XmlTextReader.get_attribute reader "target" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
          ("trigger", XmlTextReader.StartElement) -> read_trigger reader
        | ("guard", XmlTextReader.StartElement) -> read_guard reader
        | ("action", XmlTextReader.StartElement) -> read_action reader
        | ("transition", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
        | (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done





let read_deferrable reader =
  (* Parse a deferrable.

     <!ELEMENT deferrable EMPTY>
     <!ATTLIST deferrable idref IDREF #REQUIRED> *)
  assert ((XmlTextReader.name reader = "deferrable") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let deferrable = XmlTextReader.get_attribute reader "deferrable" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
          ("deferrable", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
        | (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done





let rec read_region reader =
  (* Parse a region.

     <!ELEMENT region (state*,transition* )>
     <!ATTLIST region id ID #IMPLIED> *)
  assert ((XmlTextReader.name reader = "region") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let region = XmlTextReader.get_attribute_opt reader "id" "" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
          ("region", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
        | ("state", XmlTextReader.StartElement) -> read_state reader
        | ("transition", XmlTextReader.StartElement) -> read_transition reader
        | (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done
and read_state reader =
  (* Parse a state.

     <!ELEMENT state (constraint*,deferrable*,region* )>
     <!ATTLIST state name CDATA #IMPLIED
     id ID #REQUIRED
     kind (normal|initial) "normal"> *)
  assert ((XmlTextReader.name reader = "state") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let stateid = XmlTextReader.get_attribute reader "id" in
  let statename = XmlTextReader.get_attribute_opt reader "name" "" in
  let statekind = XmlTextReader.get_attribute_opt reader "kind" "normal" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
          ("state", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
        | ("constraint", XmlTextReader.StartElement) -> read_constraint reader
        | ("deferrable", XmlTextReader.StartElement) -> read_deferrable reader
        | ("region", XmlTextReader.StartElement) -> read_region reader
        | (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done




let read_statemachine reader =
  (* Parse a state machine.

     <!ELEMENT statemachine (region+)>
     <!ATTLIST statemachine name CDATA #IMPLIED
     id ID #IMPLIED> *)
  assert ((XmlTextReader.name reader = "statemachine") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let smname = XmlTextReader.get_attribute_opt reader "name" "" in
  let smid = XmlTextReader.get_attribute_opt reader "id" "" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
          ("statemachine", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader); continue := false
        | ("region", XmlTextReader.StartElement) -> read_region reader
        | (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done





let read_associationend reader =
  (* Parse an association end.

     <!ELEMENT associationend EMPTY>
     <!ATTLIST associationend name CDATA #IMPLIED
     class CDATA #REQUIRED
     aggregation (none|aggregate|composite) "none"
     isnavigable (true|false) #IMPLIED> *)
  assert ((XmlTextReader.name reader = "associationend") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let aename = XmlTextReader.get_attribute_opt reader "name" "" in
  let aeclass = XmlTextReader.get_attribute reader "class" in
  let aggr = XmlTextReader.get_attribute_opt reader "aggregation" "none" in
  let isnav = XmlTextReader.get_attribute_opt reader "isnavigable"
    (if aename = "" then "false" else "true") in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
          ("associationend", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
        | (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done




let read_association reader =
  (* Read an association.

     <!ELEMENT association (associationend,associationend)>
     <!ATTLIST association name CDATA #IMPLIED
     export (false|true) "true"> *)
  assert ((XmlTextReader.name reader = "association") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let aname = XmlTextReader.get_attribute_opt reader "name" "" in
  let export = attribute_export reader "true" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
          ("associationend", XmlTextReader.StartElement) ->
	    read_associationend reader
        | ("association", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
        | (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_generalization reader =
  (* Read an generalization.

     <!ELEMENT generalization EMPTY>
     <!ATTLIST association parent ID #REQUIRED
     child ID #REQUIRED
     export (false|true) "true"> *)
  assert ((XmlTextReader.name reader = "generalization") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let parent = XmlTextReader.get_attribute reader "parent" in
  let child = XmlTextReader.get_attribute reader "child" in
  let export = attribute_export reader "true" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
          ("generalization", XmlTextReader.EndElement) ->
	    ignore (XmlTextReader.read reader);
	    continue := false
        | (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_initializer reader =
  (* Read an initializer from the file.

     <!ELEMENT initializer (#PCDATA)>
     <!ATTLIST initializer name CDATA #IMPLIED
     lang CDATA #REQUIRED> *)
  assert ((XmlTextReader.name reader = "initializer") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let lang = XmlTextReader.get_attribute reader "lang" in
  let name = XmlTextReader.get_attribute_opt reader "lang" "" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
          (_, XmlTextReader.Text) -> continue := XmlTextReader.read reader
        | (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
        | ("initializer", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_implementation reader =
  (* Read an implementation from the file.

     <!ELEMENT implementation (#PCDATA)>
     <!ATTLIST implementation lang CDATA #REQUIRED> *)
  assert ((XmlTextReader.name reader = "implementation") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
          (_, XmlTextReader.Text) ->
	    continue := XmlTextReader.read reader
        | (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
        | ("implementation", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_parameter reader =
  (* Read a parameter.

     <!ELEMENT parameter EMPTY>
     <!ATTLIST parameter
     name CDATA #REQUIRED
     direction (in|out|inout) "in"
     type CDATA #REQUIRED> *)
  assert ((XmlTextReader.name reader = "parameter") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let pname = XmlTextReader.get_attribute reader "name" in
  let pdir = XmlTextReader.get_attribute_opt reader "direction" "in" in
  let ptype = XmlTextReader.get_attribute reader "type" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
	  ("parameter", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
	| (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_reception reader model =
  (* Parse a reception declaration.

    <!ATTLIST reception
    name CDATA #IMPLIED
    id ID #IMPLIED
    ref IDREF #IMPLIED
    export (false|true) "true"> *)
  assert ((XmlTextReader.name reader = "reception") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let rname = XmlTextReader.get_attribute_opt reader "name" "" in
  let rid = XmlTextReader.get_attribute_opt reader "id" "" in
  let rref = XmlTextReader.get_attribute_opt reader "ref" "" in
  let rexport = attribute_export reader "true" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
          ("parameter", XmlTextReader.StartElement) ->
            read_parameter reader
        | ("constraint", XmlTextReader.StartElement) ->
            read_constraint reader
        | ("reception", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
        | (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_operation reader model =
  (* <!ELEMENT operation (parameter*,constraint*,implementation?)>
     <!ATTLIST operation
     name CDATA #REQUIRED
     type CDATA #REQUIRED
     id ID #IMPLIED
     isquery (false|true) "false"
     export (false|true) "true"> *)
  assert ((XmlTextReader.name reader = "operation") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let oname = XmlTextReader.get_attribute reader "name" in
  let otype = XmlTextReader.get_attribute reader "type" in
  let oid = XmlTextReader.get_attribute_opt reader "id" "" in
  let oisquery = 
    if (XmlTextReader.get_attribute_opt reader "isquery" "false") = "true" then
      true
    else
      false
  in
  let oexport = attribute_export reader "true" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue; do 
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
	  ("parameter", XmlTextReader.StartElement) ->
            read_parameter reader
	| ("constraint", XmlTextReader.StartElement) ->
            read_constraint reader
	| ("implementation", XmlTextReader.StartElement) ->
            read_implementation reader
	| ("operation", XmlTextReader.EndElement) -> 
            ignore (XmlTextReader.read reader);
	    continue := false
	| (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_attribute reader model =
  (* Read an attribute.

     <!ELEMENT attribute (initializer?)>
     <!ATTLIST attribute name CDATA #REQUIRED
     type CDATA #REQUIRED
     export (false|true) "true"> *)
  assert ((XmlTextReader.name reader = "attribute") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let aname = XmlTextReader.get_attribute reader "name" in
  let atype = XmlTextReader.get_attribute reader "type" in
  let export = attribute_export reader "true" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
	  ("initializer", XmlTextReader.StartElement) ->
	    read_initializer reader
	| ("attribute", XmlTextReader.EndElement) ->
	    ignore (XmlTextReader.read reader);
	    continue := false
	| (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let read_class reader model =
  (* Read a class
     <!ELEMENT class (template*,bind*,extends*,
     (attribute|operation|reception)*,constraint*,statemachine?)>
     <!ATTLIST class
     name CDATA #REQUIRED
     kind (active|passive) "passive"
     export (false|true) "true"> *)
  assert ((XmlTextReader.name reader = "class") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let name = XmlTextReader.get_attribute reader "name" in
  let kind = XmlTextReader.get_attribute_opt reader "kind" "passive" in
  let export = attribute_export reader "true" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
	  ("template", XmlTextReader.StartElement) ->
	    assert false
	| ("bind", XmlTextReader.StartElement) ->
	    assert false
	| ("extends", XmlTextReader.StartElement) ->
	    assert false
	| ("attribute", XmlTextReader.StartElement) ->
	    read_attribute reader model
	| ("operation", XmlTextReader.StartElement) ->
	    read_operation reader model
	| ("reception", XmlTextReader.StartElement) ->
	    read_reception reader model
	| ("associationend", XmlTextReader.StartElement) ->
	    read_associationend reader
	| ("constraint", XmlTextReader.StartElement) ->
	    read_constraint reader
	| ("statemachine", XmlTextReader.StartElement) ->
	    read_statemachine reader
	| ("class", XmlTextReader.EndElement) ->
	    ignore (XmlTextReader.read reader);
	    continue := false
	| (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if there is an error. *)
    done





let rec read_package reader model =
  (* Read a package.

     <!ELEMENT package ((package|class|association|generalization)* )>
     <!ATTLIST package
     name CDATA #REQUIRED
     id ID #IMPLIED> *)
  assert ((XmlTextReader.name reader = "package") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let name = XmlTextReader.get_attribute reader "name" in
  let id = XmlTextReader.get_attribute_opt reader "id" "" in
  let continue = ref (XmlTextReader.read reader) in
    while !continue  do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
	  ("package", XmlTextReader.StartElement) ->
	    read_package reader model
	| ("class", XmlTextReader.StartElement) ->
	    read_class reader model
	| ("association", XmlTextReader.StartElement) ->
	    read_association reader
	| ("generalization", XmlTextReader.StartElement) ->
	    read_generalization reader
	| ("package", XmlTextReader.EndElement) ->
	    ignore (XmlTextReader.read reader);
	    continue := false
	| (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> print_node reader; exit 1
    done





let read_head reader model =
  (* Read the head of a document and all its meta-elements.

     <!ELEMENT head (meta* )>

     <!ELEMENT meta EMPTY>
     <!ATTLIST meta
     id ID #IMPLIED
     name CDATA #REQUIRED
     value CDATA #REQUIRED> *)
  assert ((XmlTextReader.name reader = "head") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
	  ("meta", XmlTextReader.StartElement) ->
            continue := XmlTextReader.read reader
	| ("meta", XmlTextReader.EndElement) ->
	    continue := XmlTextReader.read reader
	| ("head", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
	| (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> continue := false (* Caller decides if this is an error. *)
    done





let read_model reader model =
  (* Read a model.

     <!ELEMENT suml (head?,(package|class|association|generalization)* )>
     <!ATTLIST suml
     version   CDATA #FIXED "0.2"
     timestamp CDATA #IMPLIED> *)
  assert ((XmlTextReader.name reader = "suml") &&
	    (XmlTextReader.node_type reader = XmlTextReader.StartElement));
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
	  ("head", XmlTextReader.StartElement) ->
            read_head reader model
	| ("package", XmlTextReader.StartElement) ->
            read_package reader model
	| ("class", XmlTextReader.StartElement) ->
            read_class reader model
	| ("association", XmlTextReader.StartElement) ->
            read_association reader
	| ("generalization", XmlTextReader.StartElement) ->
            read_generalization reader
	| ("suml", XmlTextReader.EndElement) ->
            ignore (XmlTextReader.read reader);
	    continue := false
	| (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader
	| _ -> print_node reader; exit 1
    done





(** Read a model in the simplified UML format from a file. *)
let from_file ~name =
  let model = Model.create name in
  let reader = XmlTextReader.from_filename name in
  let continue = ref (XmlTextReader.read reader) in
    while !continue do
      match (XmlTextReader.name reader, XmlTextReader.node_type reader) with
	  ("suml", XmlTextReader.StartElement) ->
            begin
              match XmlTextReader.get_attribute reader "version" with
                  "0.2" -> read_model reader model
                | _ -> print_node reader; exit 1
            end;
            continue := false
	| ("suml", XmlTextReader.Doctype) ->
	    continue := XmlTextReader.read reader
	| (_, XmlTextReader.SigWhitespace) ->
	    continue := XmlTextReader.read reader 
	| _ -> print_node reader; exit 1
    done;
    model
