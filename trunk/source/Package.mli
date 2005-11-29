(*
 * Package.mli -- Definition of the abstract syntax of a model.
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

(** Definition of a package. *)

(** The type of a package. *)
type package

(** Create a new, empty package. *)
val create: string -> package

(** Get the name of a package. *)
val name: package -> string

(** Add a sub-package to the package. *)
val add_package: package -> package -> unit

(** Add a classifier to the package. *)
val add_classifier: package -> Classifier.classifier -> unit
