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

let ref' = ref

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
let raw    s = View.string s

let int    = View.int
let float  = View.float
let bool   = View.bool
let char   = View.char

let seq    = View.seq
let seqa   = View.seqa

let br = raw "<br>"

let tag ?(attrs="") s p = 
  seq [raw (sprintf "<%s>" (s ^ (if attrs = "" then "" else " ") ^ attrs)); p; raw (sprintf "</%s>" s)]

let html  ?(attrs="") = tag "html"  ~attrs:attrs
let title ?(attrs="") = tag "title" ~attrs:attrs
let body  ?(attrs="") = tag "body"  ~attrs:attrs
let ul    ?(attrs="") = tag "ul"    ~attrs:attrs
let ol    ?(attrs="") = tag "ol"    ~attrs:attrs
let li    ?(attrs="") = tag "li"    ~attrs:attrs
let b     ?(attrs="") = tag "b"     ~attrs:attrs
let i     ?(attrs="") = tag "i"     ~attrs:attrs
let table ?(attrs="") = tag "table" ~attrs:attrs
let tr    ?(attrs="") = tag "tr"    ~attrs:attrs
let td    ?(attrs="") = tag "td"    ~attrs:attrs

let anchor r p = seq [raw (sprintf "<a name=%S>" r); p; raw "</a>"]
let ref    r p = seq [raw (sprintf "<a href=%S>" r); p; raw "</a>"]

let named n p = seq [b (string (n ^ ": ")); p]

let list  p = tag "ul" (seq  (List .map (tag "li") p))
let array p = tag "ul" (seqa (Array.map (tag "li") p))

let fields l = list (List.map (fun (n, x) -> named n x) l)
  
let make f x = raw (f x)

module type Element =
  sig

    type t

    val toHTML : t -> string

  end

module L = List

module String =
  struct
    
    type t = string

    let named  n v = toHTML (named n (raw v))
    let fields v   = toHTML (fields (List.map (fun (n, v) -> n, raw v) v))
    let anchor n v = toHTML (anchor n (raw v))
    let ref    n v = toHTML (ref    n (raw v))

    let toHTML = escape

  end

module Anchor (X : sig type t val name : string end) =
  struct

    module H = Hashtbl.Make 
	(
	 struct 

	   type t = X.t 

	   let hash  = Hashtbl.hash
	   let equal = (==)

	 end
	)

    let h = H.create 1024
    let index =
      let i = ref' 0 in
      (fun () -> 
	incr i;
	!i
      )

    let set x   = H.add h x (index ())
    let isSet x = H.mem h x
    let get x   = 
      if not (isSet x) then set x;
      sprintf "%s.anchor%d" X.name (H.find h x)
      
    let url t = "#" ^ get t

    let ref t text = ref (url t) text

    module String =
      struct

	let ref t text = String.ref (url t) text

      end

  end

module Raw =
  struct

    type t = string

    let toHTML s = toHTML (raw s)

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
