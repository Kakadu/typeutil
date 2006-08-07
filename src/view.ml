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

open Printf

let concatWithDelimiter delimiter acc x = match acc with "" -> x | _ -> acc ^ delimiter ^ x
let concatWithComma = concatWithDelimiter ", "
let concatWithSemicolon = concatWithDelimiter "; "

module type Viewable = 
  sig

    type t

    val toString : t -> string

  end

module type Concat =
  sig

    val concat : string -> string -> string

  end

open List

module ListC (C : Concat) (X : Viewable) =
  struct

    type t = X.t list
    let toString = fold_left (fun acc x -> C.concat acc (X.toString x)) "" 
    
  end

module List = ListC (struct let concat = concatWithComma end)

open Array

module ArrayC (C : Concat) (X : Viewable) =
  struct

    type t = X.t array
    let toString = fold_left (fun acc x -> C.concat acc (X.toString x)) ""

  end

module Array = ArrayC (struct let concat = concatWithComma end)

module SetC (C : Concat) (S : Set.S) (V : Viewable with type t = S.elt) =
  struct

    type t = S.t
    let toString x = S.fold (fun x acc -> C.concat acc (V.toString x)) x ""

  end

module Set = SetC (struct let concat = concatWithComma end)

module MapC (C : Concat) (M : Map.S) (K : Viewable with type t = M.key) (V : Viewable) =
  struct

    type t = V.t M.t
    let toString x = M.fold (fun key value acc -> 
          C.concat acc (sprintf "%s -> %s" (K.toString key) (V.toString value))
       ) x ""

  end

module Map = MapC (struct let concat = concatWithComma end)

module HashtblC (C : Concat) (M : Hashtbl.S) (K : Viewable with type t = M.key) (V : Viewable) =
  struct

    type t = V.t M.t
    let toString x = M.fold (fun key value acc -> 
          C.concat acc (sprintf "%s -> %s" (K.toString key) (V.toString value))
       ) x ""

  end

module Hashtbl = HashtblC (struct let concat = concatWithComma end)

module NamedPair (N : sig val first : string val second : string end) (F : Viewable) (S : Viewable) =
  struct

    type t = F.t * S.t

    let toString (x, y) = 
      let field value = function "" -> value | name -> sprintf "%s=%s" name value in      
      sprintf "(%s, %s)" (field (F.toString x) N.first) (field (S.toString y) N.second)

  end

module Pair = NamedPair (struct let first = "" let second = "" end)

module Char =
  struct

    type t = char
    let toString = String.make 1 

  end

module String =
  struct

    type t = string
    let toString x = x

  end

module Integer =
  struct

    type t = int
    let toString = string_of_int

  end

module Float =
  struct

    type t = float
    let toString = string_of_float

  end

module Bool =
  struct

    type t = bool
    let toString = string_of_bool

  end

module Exn =
  struct

    type t = exn
    let toString = Printexc.to_string

  end

module Unit =
  struct

    type t = unit
    let toString _ = "()"

  end

module Int32 =
  struct

    type t = int32
    let toString = Int32.to_string

  end

module Int64 =
  struct

    type t = int64
    let toString = Int64.to_string

  end

module Nativeint =
  struct

    type t = nativeint
    let toString = Nativeint.to_string

  end




