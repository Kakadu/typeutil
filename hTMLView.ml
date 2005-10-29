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
 *  (enclosed in the file LGPL).
 **************************************************************************)

open Printf

let header title = sprintf "<html>\n<title>%s</title>\n<body>\n" title
let footer = "</body>\n</html>\n"

let toHTML title body = sprintf "%s%s%s" (header title) body footer

let anchor ref text = sprintf "<a name=%s>%s</a>" ref text
let ref    ref text = sprintf "<a href=%s>%s</a>" ref text
let bold   text     = sprintf "<b>%s</b>" text
let italic text     = sprintf "<i>%s</i>" text
let br              = "\n"

let named name value = (bold (name ^ ": ")) ^ value
let fields list = 
    sprintf "<ul>\n%s\n</ul>"
       (List.fold_left (fun str (name, value) -> str ^ ("<li type=none>" ^ (named name value))) "" list)

module type Element =
  sig

    type t

    val toHTML : t -> string

  end

type generator = {append: string -> unit; contents: unit -> string}

let make () =
    let buffer = Buffer.create 1024 in
    let append = Buffer.add_string buffer in
    append "<ul>\n";
    {
      append   = (fun x -> append (sprintf "<li>%s" x));
      contents = (fun () -> append "</ul>\n"; Buffer.contents buffer);
    }

let (<@>) f g = fun x -> f (g x)

open List

module List (T : Element) =
  struct

    type t = T.t list

    let toHTML list =
        let g = make () in
        iter (g.append <@> T.toHTML) list;
        g.contents ()

  end

module Array (T : Element) =
  struct

    type t = T.t array

    let toHTML array =
        let g = make () in
        Array.iter (g.append <@> T.toHTML) array;
        g.contents ()

  end

module NamedPair (N : sig val first : string val second : string end) (F : Element) (S : Element) =
  struct

    type t = F.t * S.t

    let toHTML (f, s) = 
        let g = make () in
        g.append (sprintf "%s %s" N.first (F.toHTML f));
        g.append (sprintf "%s %s" N.second (S.toHTML s));
        g.contents ()

  end

module Pair = NamedPair (struct let first = "" let second = "" end)

module Set (S : Set.S) (V : Element with type t = S.elt) =
  struct

    type t = S.t
    let toHTML x =     
        let g = make () in
        S.iter (g.append <@> V.toHTML) x;
        g.contents ()

  end

module Map (M : Map.S) (K : Element with type t = M.key) (V : Element) =
  struct

    type t = V.t M.t
    let toHTML x =     
        let g = make () in
        M.iter (fun key value -> 
           let module P = NamedPair(struct let first = bold "key" let second = bold "value" end)(K)(V) in 
           g.append (P.toHTML (key, value))
        ) x;
        g.contents ()

  end

module Hashtbl (M : Hashtbl.S) (K : Element with type t = M.key) (V : Element) =
  struct

    type t = V.t M.t
    let toHTML x =     
        let g = make () in
        M.iter (fun key value -> 
           let module P = NamedPair(struct let first = bold "key" let second = bold "value" end)(K)(V) in 
           g.append (P.toHTML (key, value))
        ) x;
        g.contents ()

  end

module String =
  struct
    
    type t = string
	  
    let toHTML s = s

  end
