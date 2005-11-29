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
  print_endline ((XmlReader.name reader) ^ ": " ^
		   XmlReader.string_of_nodetype (XmlReader.node_type reader))





let read_attribute reader model =
  let continue = ref true in
    while !continue && XmlReader.read reader do
      let name = XmlReader.name reader in
      let node_type = XmlReader.node_type reader in
	match (name, node_type) with
	  | ("initializer", XmlReader.StartElement) -> ()
	  | ("attribute", XmlReader.EndElement) -> continue := false
	  | (_, XmlReader.SigWhitespace) -> ()
	  | _ -> continue := false; print_node reader
    done





let read_operation reader model =
  let continue = ref true in
    while !continue && XmlReader.read reader do
      let name = XmlReader.name reader in
      let node_type = XmlReader.node_type reader in
	match (name, node_type) with
	  | ("parameter", XmlReader.StartElement) -> ()
	  | ("constraint", XmlReader.StartElement) -> ()
	  | ("implementation", XmlReader.StartElement) -> ()
	  | ("operation", XmlReader.EndElement) -> continue := false
	  | ("class", XmlReader.EndElement) -> continue := false
	  | (_, XmlReader.SigWhitespace) -> ()
	  | _ -> print_node reader; continue := false
    done





let read_reception reader model =
  let continue = ref true in
    while !continue && XmlReader.read reader do
      let name = XmlReader.name reader in
      let node_type = XmlReader.node_type reader in
	match (name, node_type) with
	  | ("parameter", XmlReader.StartElement) -> ()
	  | ("constraint", XmlReader.StartElement) -> ()
	  | ("reception", XmlReader.EndElement) -> continue := false
	  | ("class", XmlReader.EndElement) -> continue := false
	  | (_, XmlReader.SigWhitespace) -> ()
	  | _ -> print_node reader; continue := false
    done





let read_class reader model =
  let continue = ref true in
    while !continue && XmlReader.read reader do
      let name = XmlReader.name reader in
      let node_type = XmlReader.node_type reader in
	match (name, node_type) with
	    ("template", XmlReader.StartElement) -> ()
	  | ("bind", XmlReader.StartElement) -> ()
	  | ("extends", XmlReader.StartElement) -> ()
	  | ("attribute", XmlReader.StartElement) ->
	      read_attribute reader model
	  | ("operation", XmlReader.StartElement) ->
	      read_operation reader model
	  | ("association", XmlReader.StartElement) -> ()
	  | ("constraint", XmlReader.StartElement) -> ()
	  | ("statemachine", XmlReader.StartElement) -> ()
	  | ("class", XmlReader.EndElement) -> continue := false
	  | (_, XmlReader.SigWhitespace) -> ()
	  | _ -> print_node reader; continue := false
    done





let rec read_package reader model =
  let continue = ref true in
    while !continue && XmlReader.read reader do
      let name = XmlReader.name reader in
      let node_type = XmlReader.node_type reader in
	match (name, node_type) with
	    ("package", XmlReader.StartElement) -> read_package reader model
	  | ("class", XmlReader.StartElement) -> read_class reader model
	  | ("association", XmlReader.StartElement) -> ()
	  | ("generalisation", XmlReader.StartElement) -> ()
	  | ("package", XmlReader.EndElement) -> continue := false
	  | (_, XmlReader.SigWhitespace) -> ()
	  | _ -> print_node reader; continue := false
    done





let read_head reader model =
  let continue = ref true in
    while !continue && XmlReader.read reader do
      let name = XmlReader.name reader in
      let node_type = XmlReader.node_type reader in
	match name with
	    "meta" when node_type = XmlReader.StartElement -> ()
	  | "head" when node_type = XmlReader.EndElement -> continue := false
	  | "suml" when node_type = XmlReader.EndElement -> continue := false
	  | _ when node_type = XmlReader.SigWhitespace -> ()
	  | _ -> print_node reader; continue := false
    done





let read_model reader model =
  let continue = ref true in
    while !continue && XmlReader.read reader do
      let name = XmlReader.name reader in
      let node_type = XmlReader.node_type reader in
	match (name, node_type) with
	    ("head", XmlReader.StartElement) -> read_head reader model
	  | ("package", XmlReader.StartElement) -> read_package reader model
	  | ("class", XmlReader.StartElement) -> read_class reader model
	  | ("association", XmlReader.StartElement) -> ()
	  | ("generalization", XmlReader.StartElement) -> ()
	  | ("suml", XmlReader.EndElement) -> continue := false
	  | (_, XmlReader.SigWhitespace) -> ()
	  | _ -> print_node reader; continue := false
    done





let read_document reader model =
  let continue = ref true in
    while !continue && XmlReader.read reader do
      let name = XmlReader.name reader in
      let node_type = XmlReader.node_type reader in
	match (name, node_type) with
	    ("suml", XmlReader.StartElement) ->
	      begin
		match XmlReader.get_attribute reader "version" with
                    "0.2" -> read_model reader model
		  | _ -> print_node reader; continue := false
	      end
	  | ("suml", XmlReader.EndElement) -> ()
	  | ("suml", XmlReader.Doctype) -> ()
	  | (_, XmlReader.SigWhitespace) -> ()
	  | _ -> print_node reader; continue := false
    done





let from_file name =
  let model = Model.create name in
    read_document (XmlReader.from_filename name) model;
    model
