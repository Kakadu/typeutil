(**************************************************************************
 *  Copyright (C) 2005-2008
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

module type Comparable =
  sig

    type t 
    val compare : t -> t -> int

  end

module List (C : Comparable) =
  struct

     type t = C.t list

     let rec compare x y =
        (function
         | [], [] ->  0
         | [],  _ -> -1
         |  _, [] ->  1
         | x :: xs, y :: ys -> match C.compare x y with 0 -> compare xs ys | x -> x
        ) (x, y)

  end

module Pair (X : Comparable) (Y : Comparable) =
  struct

     type t = X.t * Y.t

     let compare (x, z) (y, t) = 
         match X.compare x y with
         | 0 -> Y.compare z t
         | x -> x

  end

module Map (Key : Comparable) (Value : Comparable) =
  struct

    type t = Value.t Map.Make(Key).t

    let compare x y = let module M = Map.Make(Key) in M.compare Value.compare x y        

  end

open Array

module Array (X : Comparable) =
  struct

    type t = X.t array

    let compare x y =
        let n, m = length x, length y in
        let rec inner i j =
            if i < n 
            then if j < m then match X.compare x.(i) y.(j) with 0 -> inner (i+1) (j+1) | x -> x else 1
            else if j < m then -1 else 0
        in inner 0 0 

  end

module C (X : sig type t end) = struct type t = X.t let compare = compare end

module Char = C (struct type t = char end)
module String = C (struct type t = string end)
module Integer = C (struct type t = int end)
module Float = C (struct type t = float end)
module Bool = C (struct type t = bool end)
module Exn = C (struct type t = exn end)
module Unit = C (struct type t = unit end)
module Int32 = C (struct type t = int32 end)
module Int64 = C (struct type t = int64 end)
module Nativeint = C (struct type t = nativeint end)



