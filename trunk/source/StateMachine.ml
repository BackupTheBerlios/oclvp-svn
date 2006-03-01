(*
 * StateMachine.ml -- Definition of the abstract syntax of state machines.
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

module Action = struct

  type t =
      None
    | Opaque of string * string

  let create lang action = Opaque (lang, action)

end





module Guard = struct

  type t =
      None
    | Opaque of string * string
    | Ocl of string * OCL.oclast

  let create_opaque lang guard = Opaque (lang, guard)

  let create lang guard = Ocl (lang, guard)

end





module Trigger = struct

  type t = string option

  let create trigger = Some trigger

end





module Transition = struct

  type t = {
    source: string;
    name: string;
    trigger: Trigger.t;
    guard: Guard.t;
    action: Action.t;
    target: string;
  }

  let create s n tr g a tg =
    { source = s; name = n; trigger = tr; guard = g; action = a; target = tg }

end





module State = struct

  type t = { name: string }

  let create n = { name = n }

end

type t =
    { states: State.t list;
      transitions: Transition.t list;
    }

let create () = { states = []; transitions = [] }
