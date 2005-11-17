(*
 * XmlReader.ml -- OCaml bindings for libxml's XmlTextReader.
 *
 * This file is part of oclvp
 *
 * Copyright (c) 2005 by Marcel Kyas <mkyas@users.berlios.de>
 *
 * Derived from ocaml-xmlr - OCaml bindings for libxml's xmlreader.
 * Copyright (C) 2004  Evan Martin <martine@danga.com>
 *
 * License changed from LGPL to GPL for this modified version.
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

type xmlreader

type nodetype = NodeTypeNone | StartElement | Attribute | Text | CData
        (* 5*)| EntityRef | EntityDecl | PI | Comment | Document
        (*10*)| Doctype | Fragment | Notation | Whitespace | SigWhitespace
        (*15*)| EndElement | EndEntity | XMLDecl

let string_of_nodetype = function
    NodeTypeNone -> "None"
  | StartElement -> "StartElement"
  | Attribute    -> "Attribute"
  | Text         -> "Text"
  | CData        -> "CData"
  | EntityRef    -> "EntityRef"
  | EntityDecl   -> "EntityDecl"
  | PI           -> "PI"
  | Comment      -> "Comment"
  | Document     -> "Document"
  | Doctype      -> "Doctype"
  | Fragment     -> "Fragment"
  | Notation     -> "Notation"
  | Whitespace   -> "Whitespace"
  | SigWhitespace-> "SigWhitespace"
  | EndElement   -> "EndElement"    
  | EndEntity    -> "EndEntity"    
  | XMLDecl      -> "XMLDecl"    

type parser_option = Recover | NoEnt | DTDLoad | DTDAttr | DTDValid |
                     NoError | NoWarning | Pedantic | NoBlanks | SAX1 |
                     XInclude | NoNet | NoDict | NSClean | NoCDATA

external from_filename : ?encoding:string -> ?opts:parser_option list -> string -> xmlreader = "xml_reader_from_filename"

external from_string : ?baseurl:string -> ?encoding:string -> ?opts:parser_option list -> string -> xmlreader = "xml_reader_from_string"

external read : xmlreader -> bool = "xml_reader_read"
external next : xmlreader -> bool = "xml_reader_next"

external node_type : xmlreader -> nodetype = "xml_reader_nodetype"
external prefix : xmlreader -> string = "xml_reader_prefix"
external local_name : xmlreader -> string = "xml_reader_local_name"
external name : xmlreader -> string = "xml_reader_name"
external namespace_uri : xmlreader -> string = "xml_reader_namespace_uri"

external has_value : xmlreader -> bool = "xml_reader_has_value"
external value : xmlreader -> string = "xml_reader_value"

external base_uri : xmlreader -> string = "xml_reader_base_uri"

external is_empty_element : xmlreader -> bool = "xml_reader_is_empty_element"
external depth : xmlreader -> int = "xml_reader_depth"

(* XXX doesn't work--unimplemented in libxml itself.
(** Reads the contents of the current node, including child nodes and markup.
    Returns a string containing the XML content. *)
external inner_xml : xmlreader -> string = "xml_reader_inner_xml"
external outer_xml : xmlreader -> string = "xml_reader_outer_xml"
*)

external has_attributes : xmlreader -> bool = "xml_reader_has_attributes"

external attribute_count : xmlreader -> int = "xml_reader_attribute_count"

external get_attribute : xmlreader -> string -> string = "xml_reader_get_attribute"

external get_attribute_no : xmlreader -> int -> string = "xml_reader_get_attribute_no"

external get_attribute_ns : xmlreader -> string -> string -> string = "xml_reader_get_attribute_ns"

let skip_to_close (r : xmlreader) (tag : string) : unit =
  if is_empty_element r then () else
  let rec skip_more () =
    if node_type r = EndElement &&
       name r = tag then
      ()
    else
      if not (read r) then raise Not_found
      else skip_more ()
  in skip_more ()

let fold_subnodes (r : xmlreader) (tag : string) (initial : 'a) 
    (f : string -> 'a -> 'a) : 'a =
  let rec read_more state =
    let nt = node_type r in
    (* stop here if we're done. *)
    if nt = EndElement && name r = tag then state else
    (* let this node update our state *)
    let newstate = 
      if nt != StartElement then state else begin
        let thistag = name r in
        let newstate = f thistag state in
        (* verify that the function read through to the closing tag. *)
        if node_type r != EndElement ||
           name r != thistag then
          skip_to_close r thistag;
        newstate
      end
    in
    (* and try to advance to the next *)
    if read r then
      read_more newstate
    else
      newstate
  in
  ignore (read r);
  read_more initial

let iter_subnodes (r : xmlreader) (tag : string) (f : string -> unit) : unit =
  fold_subnodes r tag () (fun tag _ -> f tag)
