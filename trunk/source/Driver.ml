(*
 * Driver.ml -- The main routine.
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

open Arg;;





(** Show the name and the version of the program and exit. *)
let show_version () =
  print_string (Version.oclvp_package ^ " " ^ Version.oclvp_version ^ "\n") ;
  print_string "Copyright (c) 2005 Marcel Kyas\n";
  print_string "This is free software; see the source for copying conditions.\n";
  print_string "There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A\n";
  print_string "PARTICULAR PURPOSE.\n";
  exit 0;;

let ignore x = ()

let ocl_from_file name =
  let tree = OCL.from_file name in
    if !Settings.dump_to_xml then
      begin
	print_endline "Here!";
	OCL.unit_to_xml (XmlTextWriter.to_file "-" 0) tree
      end
    else
      ()

let ocl_from_string expr =
  let tree = OCL.from_string expr in
    if !Settings.dump_to_xml then
      begin
	let writer = XmlTextWriter.to_file "-" 0 in
	  XmlTextWriter.set_indent writer true;
	  OCL.unit_to_xml writer tree
      end
    else
      ()




let from_file name =
  let len = String.length name in
  match (String.sub name (len - 4) 4) with
    ".ocl" -> ignore (ocl_from_file name)
  | "suml" -> ignore (SUML.from_file name)
  | ".xmi" -> ignore (XMI.from_file name)
  | _ -> assert false
  ;;

let options = [
  ("-ocl", String ocl_from_file, "file  Read an OCL file");
  ("-oclexp", String ocl_from_string,
   "string  Parse string as the contents of an OCL file.");
  ("-suml", String (function f -> ignore (SUML.from_file f)),
   "file  Read a Simplified UML file");
  ("-xmi", String (function f -> ignore (XMI.from_file f)),
   "file  Read an XMI file");
  ("-dump", Set Settings.dump_to_xml, "  Write the tree to XML after parsing");
  ("-keep-temps", Set Settings.keep_temps, "  Keep temporary files.");
  ("-v", Unit (function () -> ()),
   "  Print some information while processing");
  ("-V", Unit show_version,
   "  Show the version and exit");
  ("--version", Unit show_version, "  Show the version and exit")]
;;

let usage = Sys.executable_name ^ " [options]" ;;



let main () =
  parse options from_file usage ;
  exit 0;;

main() ;;
