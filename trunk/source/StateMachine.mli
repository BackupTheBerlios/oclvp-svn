(*
 * StateMachine.mli -- Interface to the abstract syntax of a state machine.
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
module Action: sig

  type t =
      None
    | Opaque of string * string

  val create: string -> string -> t

end





module Guard: sig

  type t =
      None
    | Opaque of string * string
    | Ocl of string * OCL.oclast

  val create_opaque: string -> string -> t

  val create: string -> OCL.oclast -> t

end






module Trigger: sig

  type t = string option

  val create: string -> t

end





module Transition: sig

  type t

  val create: string -> string -> Trigger.t -> Guard.t -> Action.t -> string -> t

end





module State: sig

  type t

  val create: string -> t

end





type t

val create: unit -> t
