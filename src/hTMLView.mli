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

(** {1 Viewing values of various types in HTML-format} *)

(** {2 Combinator interface} *)

(** Type synonym for viewer function to be referenced as [HTMLView.er] *)
type er = View.er

(** Type synonym to be referenced unqualified*)
type viewer = er

(** String conversion *)
val toHTML : viewer -> string

(** Escapes special HTML symbols ("<", ">", "&", """") *)
val escape : string -> string

(** Escaped string *)
val string : string -> viewer

(** Raw string *)
val raw : string -> viewer

(** {3 Viewer constructors for build-in types} *)

val int : int -> viewer
val float : float -> viewer
val bool : bool -> viewer
val char : char -> viewer

(** {3 Sequence constructors} *)

val seq : viewer list -> viewer
val seqa : viewer array -> viewer

(** {3 Some predefined HTML-specific viewers} *)

(** [anchor ref p] outputs [p] within the anchor [ref] *)
val anchor : string -> viewer -> viewer

(** [ref ref p] outputs [p] as hyper-reference to [ref] *)
val ref : string -> viewer -> viewer

(** [named name p] outputs [p] as named by [name] item *)
val named : string -> viewer -> viewer

(** Outputs unordered list *)
val list : viewer list -> viewer

(** Outputs unordered list *)
val array : viewer array -> viewer

(** Outputs a list of named elements *)
val fields : (string * viewer) list -> viewer

(** Break viewer *)
val br : viewer

(** Tagged viewer: [tag name p] surrounds [p] with open and close tags 
    with name [name]
*)
val tag : string -> viewer -> viewer

(** {3 Some tags} *)

val html : viewer -> viewer
val title : viewer -> viewer
val body : viewer -> viewer
val ul : viewer -> viewer
val li : viewer -> viewer
val b : viewer -> viewer
val i : viewer -> viewer

(** {2 Helper module to provide anchors to values} *)

module Anchor (X : sig type t end) :
  sig

    (** Set anchor for value *)
    val set : X.t -> unit

    (** Checks whether anchor is set *)
    val isSet : X.t -> bool

    (** Get anchor value. Raises [Not_found] if no anchor is set *)
    val get : X.t -> string

  end

(** {2 Functorial interface} *)

(** An abstract element to generate HTML from *)
module type Element =
  sig 

    (** The type *)
    type t 

    (** Generate HTML representation *)    
    val toHTML : t -> string 

  end

(** Functor to provide list to HTML generation *)
module List (T : Element) : Element with type t = T.t list

(** Functor to provide array to HTML generation *)
module Array (T : Element) : Element with type t = T.t array

(** Functor to provide set to HTML generation. 
    Set items are ordered in according to their <b>string representations</b> 
*)
module Set (S : Set.S) (V : Element with type t = S.elt) : Element with type t = S.t

(** Functor to provide map to HTML generation. 
    Set items are ordered in according to their <b>string representations</b> 
*)
module Map (M : Map.S) (K : Element with type t = M.key) (V : Element) : Element with type t = V.t M.t

(** Functor to provide hashtable to HTML generation. 
    Set items are ordered in according to their <b>string representations</b> 
*)
module Hashtbl (M : Hashtbl.S) (K : Element with type t = M.key) (V : Element) : Element with type t = V.t M.t

(** Functor to provide named pair to HTML generation. The first parameter sets components names *)
module NamedPair (N : sig val first : string val second : string end) (F : Element) (S : Element) : Element with 
  type t = F.t * S.t

(** Functor to provide unnamed pair to HTML generation *)
module Pair (F : Element) (S : Element) : Element with type t = F.t * S.t

(** Module to provide raw string HTML generation *)
module Raw : Element with type t = string

(** Module to provide string to HTML generation *)
module String :
  sig

    (** Type synonym *)
    type t = string

    (** HTML viewer *)
    val toHTML : string -> string

    (** Synonym for [named] for string values *)
    val named  : string -> string -> string

    (** Synonym for [fields] for string values *)
    val fields : (string * string) list -> string

    (** Synonym for [anchor] for string values *)
    val anchor : string -> string -> string

    (** Synonym for [ref] for string values *)
    val ref : string -> string -> string

  end
