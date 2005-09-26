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

(** Functorial constructors to provide string conversion functions for the
    standard collection types *)

(** Signature to supply viewing function *)
module type Viewable = 
  sig

    (** Principal type *)
    type t

    (** View function *)
    val toString : t -> string

  end

(** Signature to supply concatenation function *)
module type Concat =
  sig
 
    (** Concatenate *)
    val concat : string -> string -> string

  end

(** Viewing lists of {!Viewable} types with explicit concatenation function *)
module ListC (C : Concat) (X : Viewable) : Viewable with type t = X.t list

(** Viewing arrays of {!Viewable} types with explicit concatenation function *)
module ArrayC (C : Concat) (X : Viewable) : Viewable with type t = X.t array

(** Viewing sets of {!Viewable} types with explicit concatenation function *)
module SetC (C : Concat) (S : Set.S) (V : Viewable with type t = S.elt) : Viewable with type t = S.t

(** Viewing maps of {!Viewable} types with explicit concatenation function *)
module MapC (C : Concat) (M : Map.S) (K : Viewable with type t = M.key) (V : Viewable) : Viewable with type t = V.t M.t

(** Viewing hashtables of {!Viewable} types with explicit concatenation function *)
module HashtblC (C : Concat) (M : Hashtbl.S) (K : Viewable with type t = M.key) (V : Viewable) : Viewable with type t = V.t M.t

(** Viewing lists of {!Viewable} types with concatenation with comma *)
module List (X : Viewable) : Viewable with type t = X.t list

(** Viewing arrays of {!Viewable} types with concatenation with comma *)
module Array (X : Viewable) : Viewable with type t = X.t array

(** Viewing sets of {!Viewable} types with concatenation with comma *)
module Set (S : Set.S) (V : Viewable with type t = S.elt) : Viewable with type t = S.t

(** Viewing maps of {!Viewable} types with concatenation with comma *)
module Map (M : Map.S) (K : Viewable with type t = M.key) (V : Viewable) : Viewable with type t = V.t M.t

(** Viewing hashtables of {!Viewable} types with concatenation with comma *)
module Hashtbl (M : Hashtbl.S) (K : Viewable with type t = M.key) (V : Viewable) : Viewable with type t = V.t M.t

(** Viewing named pairs. The first parameter supplies components names *)
module NamedPair (N : sig val first : string val second : string end) (F : Viewable) (S : Viewable) : 
   Viewable with type t = F.t * S.t

(** Viewing unnamed pairs *)
module Pair (F : Viewable) (S : Viewable) : Viewable with type t = F.t * S.t

(** Wrapper to make string viewable *)
module String : Viewable with type t = string

(** {2 Viewing helpers} *)

(** Concatenation function: [concatWithDelimiter x y delim] 
    returns [x ^ delim ^ y] if x is not empty and [y] otherwise *)
val concatWithDelimiter : string -> string -> string -> string

(** Concatenation with comma *)
val concatWithComma : string -> string -> string

(** Concatenation with semicolon *)
val concatWithSemicolon : string -> string -> string
