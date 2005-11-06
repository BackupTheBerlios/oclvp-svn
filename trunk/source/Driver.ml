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

let show_version () =
  print_string (Version.oclvp_package ^ " " ^ Version.oclvp_version) ;
  print_newline () ;
  exit 0;;

let load_file (x: string) =
    ()
;;

let options = [
  ("-V", Unit show_version, "Show the version and exit");
  ("--version", Unit show_version, "Show the version and exit")
];;

let usage = "TBD"
;;



let main () =
  parse options load_file usage ;
  exit 0;;

main() ;;
