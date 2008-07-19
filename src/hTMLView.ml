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

open Printf

type er = View.er
type viewer = er

let toHTML = View.toString

let escape s =
  let buf = Buffer.create (String.length s * 2) in
  for i=0 to String.length s - 1 do
    Buffer.add_string buf
      (match s.[i] with
      | '<' -> "&lt;"
      | '>' -> "&gt;"
      | '&' -> "&amp;"
      | '"' -> "&quot;"
      | c   -> String.make 1 c
      )
  done;
  Buffer.contents buf

let string s = View.string (escape s)

let int    = View.int
let float  = View.float
let bool   = View.bool
let char   = View.char

let seq    = View.seq
let seqa   = View.seqa

let br = string "<br>"

let tag s p = seq [string (sprintf "<%s>" s); p; string (sprintf "</%s>" s)]

let html    = tag "html"
let title   = tag "title"
let body    = tag "body"
let ul      = tag "ul"
let li      = tag "li"
let b       = tag "b"
let i       = tag "i"

let anchor r p = seq [string (sprintf "<a name=\"%S\">" r); p; string "</a>"]
let ref    r p = seq [string (sprintf "<a href=\"%S\">" r); p; string "</a>"]

let named n p = seq [b (string (n ^ ": ")); p]

let list  p = tag "ul" (seq  (List .map (tag "li") p))
let array p = tag "ul" (seqa (Array.map (tag "li") p))

let fields l = list (List.map (fun (n, x) -> named n x) l)
  
let make f x = string (f x)

module type Element =
  sig

    type t

    val toHTML : t -> string

  end

module L = List

module String =
  struct
    
    type t = string

    let toHTML = escape

  end

open List

module List (T : Element) =
  struct

    type t = T.t list

    let toHTML l = toHTML (list (List.map (make T.toHTML) l))

  end

module Array (T : Element) =
  struct

    type t = T.t array

    let toHTML a = toHTML (array (Array.map (make T.toHTML) a))

  end

module NamedPair (N : sig val first : string val second : string end) (F : Element) (S : Element) =
  struct

    type t = F.t * S.t

    let toHTML (f, s) = 
      toHTML
	(list 
	   [named N.first  (make F.toHTML f);
            named N.second (make S.toHTML s);
	   ]
	)

  end

module Pair = NamedPair (struct let first = "" let second = "" end)

module Set (S : Set.S) (V : Element with type t = S.elt) =
  struct

    type t = S.t

    let toHTML x =     
      let module LL = List (String) in
      LL.toHTML (L.sort compare (L.map V.toHTML (S.elements x)))

  end

module Map (M : Map.S) (K : Element with type t = M.key) (V : Element) =
  struct

    type t = V.t M.t

    let toHTML x =     
      let module P  = NamedPair (struct let first = "key" let second = "value" end)(K)(V) in 
      let module LL = List (String) in
      LL.toHTML (L.sort compare (M.fold (fun x y acc -> (P.toHTML (x, y)) :: acc) x []))

  end

module Hashtbl (M : Hashtbl.S) (K : Element with type t = M.key) (V : Element) =
  struct

    type t = V.t M.t

    let toHTML x =     
      let module P  = NamedPair(struct let first = "key" let second = "value" end)(K)(V) in 
      let module LL = List (String) in
      LL.toHTML (L.sort compare (M.fold (fun x y acc -> (P.toHTML (x, y)) :: acc) x []))

  end
