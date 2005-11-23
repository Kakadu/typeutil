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

(** Functorial constructors to provide HTML generation functions for the
    standard collection types *)

(** An abstract element to generate HTML from *)
module type Element =
  sig 

    (** Principal type *)
    type t 

    (** Generate HTML representation *)    
    val toHTML : t -> string 

  end

(** Functor to provide list to HTML generation *)
module List (T : Element) : Element with type t = T.t list

(** Functor to provide array to HTML generation *)
module Array (T : Element) : Element with type t = T.t array

(** Functor to provide set to HTML generation *)
module Set (S : Set.S) (V : Element with type t = S.elt) : Element with type t = S.t

(** Functor to provide map to HTML generation *)
module Map (M : Map.S) (K : Element with type t = M.key) (V : Element) : Element with type t = V.t M.t

(** Functor to provide hashtable to HTML generation *)
module Hashtbl (M : Hashtbl.S) (K : Element with type t = M.key) (V : Element) : Element with type t = V.t M.t

(** Functor to provide named pair to HTML generation. The first parameter sets components names *)
module NamedPair (N : sig val first : string val second : string end) (F : Element) (S : Element) : Element with type t = F.t * S.t

(** Functor to provide unnamed pair to HTML generation. *)
module Pair (F : Element) (S : Element) : Element with type t = F.t * S.t

(** Module to provide string to HTML generation. *)
module String : Element with type t = string

(** {2 Miscellaneous helpers} *)

(** Generate HTML header. [header title] generates header of HTML file
    with title [title] *)
val header : string -> string

(** HTML footer *)
val footer : string

(** Generate HTML. [toHTML title body] generates HTML file with title [title] and body [body] *)
val toHTML : string -> string -> string 

(** Generate anchor in the HTML text. [anchor ref text] generates [text] suppounded by
    corresponding [<a name=...>...</a>] tags *)
val anchor : string -> string -> string

(** Generate hyperlink reference. [ref ref text] generates [text] surrounded by
    corresponding [<a href=...>...</a>] tags *)
val ref : string -> string -> string

(** Generate [text] in bold *)
val bold : string -> string

(** Generate [text] in italic *)
val italic : string -> string

(** HTML line break *)
val br : string

(** Generate named value. [named name value] generates the string 
    ["<b>name:</b> value"] *)
val named : string -> string -> string

(** Generate list of named values separated by [br] *)
val fields : (string * string) list -> string 
