(*
 * Package.ml -- Definition of the abstract syntax of a package.
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

type package = { package_name: string;
	         package_packages: (string, package) Hashtbl.t }

let name package = package.package_name

let create name =
  { package_name = name; package_packages = Hashtbl.create 17 }

let add_package pkg package =
  Hashtbl.add pkg.package_packages (name package) package

let add_classifier pkg classifier =
  ()
