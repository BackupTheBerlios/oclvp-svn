(* ocaml-xmlr - OCaml bindings for libxml's xmlreader.
 * Copyright (C) 2004  Evan Martin <martine@danga.com>
 *)

(*
 * xmlr_stubs.c -- OCaml bindings for libxml's XmlReader.
 *
 * This file has been modified and adapted for use in oclvp
 * by Marcel Kyas <mkyas@users.berlios.de> on November 17, 2005.
 *
 * Contrary to the other files of oclvp, the following license applies to
 * the content of this file and xmlr_stubs.c
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS OF THE
 * SOFTWARE.
 *
 * Except as contained in this notice, the names of the authors shall not
 * be used in advertising or otherwise to promote the sale, use or other
 * dealings in this Software without prior written authorisation of the
 * authors.
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
