(*
 * XMI.ml -- Try to read an XMI file.
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

let stylesheet =
  let name = "xmi2suml.xsl" in
    if Sys.file_exists name
    then name
    else
      let path =
	try
	  Sys.getenv "OCLVP_DATA"
	with
	    Not_found -> Version.oclvp_datadir
      in
	Filename.concat path name

let from_file file =
  let target = Filename.temp_file "oclvp" ".suml" in
    Xml.to_file
      (Xslt.transform (Xslt.parse_stylesheet stylesheet) (Xml.from_file file))
      target true;
    let result = SUML.from_file target in
      if not !Settings.keep_temps then (Sys.remove target);
      result
