(**************************************************************************
 *  Copyright (C) 2005
 *  Dmitri Boulytchev (db@tepkom.ru), St.Petersburg State University
 *  Universitetskii pr., 28, St.Petersburg, 198504, RUSSIA    
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 *
 *  See the GNU Lesser General Public License version 2.1 for more details
 *  (enclosed in the file COPYING).
 **************************************************************************)

(** Functorial constructors to provide compare functions for the
    standard collection types *)

(** Comparable signature *)
module type Comparable = 
  sig 

    (** Principal type *)
    type t 

    (** Compare function *)
    val compare : t -> t -> int 

  end

(** Comparator for lists *)
module List  (X   : Comparable) : Comparable with type t = X.t list

(** Comparator for arrays *)
module Array (X   : Comparable) : Comparable with type t = X.t array

(** Comparator for pairs *)
module Pair  (X   : Comparable) (Y    : Comparable) : Comparable with type t = X.t * Y.t

(** Comparator for maps *)
module Map   (Key : Comparable) (Value : Comparable) : Comparable with type t = Value.t Map.Make(Key).t

(** Wrapper to make builtin types compatable *)
module String : Comparable with type t = string
module Integer : Comparable with type t = int
module Float : Comparable with type t = float
module Bool : Comparable with type t = bool
module Char : Comparable with type t = char
module Unit : Comparable with type t = unit
module Exn : Comparable with type t = exn
module Int32 : Comparable with type t = int32
module Int64 : Comparable with type t = int64
module Nativeint : Comparable with type t = nativeint
