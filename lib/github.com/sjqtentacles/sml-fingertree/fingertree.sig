(* fingertree.sig

   2-3 finger trees (Hinze & Paterson) giving a general-purpose persistent
   sequence / deque, in pure Standard ML.

   A finger tree supports amortised O(1) access and update at both ends (`cons`,
   `snoc`, `viewL`, `viewR`), O(log n) concatenation (`append`), positional
   lookup (`index`), and split (`splitAt`), via a cached size measure at every
   node. All operations are *persistent*: they return new trees and never mutate
   their arguments. No FFI, threads, clock or randomness: the same inputs always
   produce the same outputs under MLton and Poly/ML.

   `viewL` / `viewR` decompose a sequence from the left / right respectively,
   returning `NONE` for the empty sequence. Positions for `index` and `splitAt`
   are 0-based. *)

signature FINGERTREE =
sig
  type 'a t

  val empty   : 'a t
  val isEmpty : 'a t -> bool
  val length  : 'a t -> int

  val cons    : 'a -> 'a t -> 'a t              (* push onto the front *)
  val snoc    : 'a -> 'a t -> 'a t              (* push onto the back *)

  val viewL   : 'a t -> ('a * 'a t) option      (* uncons from the front *)
  val viewR   : 'a t -> ('a * 'a t) option      (* uncons from the back *)
  val head    : 'a t -> 'a                       (* first; raises Empty if empty *)
  val last    : 'a t -> 'a                       (* last; raises Empty if empty *)

  val append  : 'a t -> 'a t -> 'a t

  val index   : 'a t -> int -> 'a               (* 0-based; raises Subscript *)
  val splitAt : 'a t -> int -> 'a t * 'a t      (* first i elements, rest *)

  val fromList : 'a list -> 'a t
  val toList   : 'a t -> 'a list
end
