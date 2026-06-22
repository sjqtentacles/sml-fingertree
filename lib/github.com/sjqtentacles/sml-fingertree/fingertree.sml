(* fingertree.sml - 2-3 finger trees (Hinze & Paterson) with a size measure.

   To stay within Standard ML's monomorphic recursion, elements are wrapped in a
   single uniform `node` type (`Leaf` at the bottom, `Node2`/`Node3` deeper) so
   the spine type `'a ft` is *not* nested (`'a ft`, never `'a node ft`). The
   level invariant -- the top spine carries only `Leaf`s, the middle only
   `Node2`/`Node3` -- is maintained internally. Every node and Deep caches its
   element count, giving O(log n) `index`/`splitAt` and O(log n) `append`. *)

structure Fingertree :> FINGERTREE =
struct

  datatype 'a node =
      Leaf of 'a
    | Node2 of int * 'a node * 'a node
    | Node3 of int * 'a node * 'a node * 'a node

  datatype 'a digit =
      One   of 'a node
    | Two   of 'a node * 'a node
    | Three of 'a node * 'a node * 'a node
    | Four  of 'a node * 'a node * 'a node * 'a node

  datatype 'a ft =
      Empty
    | Single of 'a node
    | Deep of int * 'a digit * 'a ft * 'a digit

  type 'a t = 'a ft

  (* ---- measures ---- *)
  fun nsize (Leaf _) = 1
    | nsize (Node2 (s, _, _)) = s
    | nsize (Node3 (s, _, _, _)) = s

  fun mkNode2 (a, b) = Node2 (nsize a + nsize b, a, b)
  fun mkNode3 (a, b, c) = Node3 (nsize a + nsize b + nsize c, a, b, c)

  fun dsize (One a) = nsize a
    | dsize (Two (a, b)) = nsize a + nsize b
    | dsize (Three (a, b, c)) = nsize a + nsize b + nsize c
    | dsize (Four (a, b, c, d)) = nsize a + nsize b + nsize c + nsize d

  fun ftsize Empty = 0
    | ftsize (Single x) = nsize x
    | ftsize (Deep (s, _, _, _)) = s

  fun mkDeep (pr, m, sf) = Deep (dsize pr + ftsize m + dsize sf, pr, m, sf)

  val empty = Empty
  fun isEmpty Empty = true | isEmpty _ = false
  fun length t = ftsize t

  fun unLeaf (Leaf a) = a
    | unLeaf _ = raise Fail "fingertree: level invariant violated"

  fun digitToList (One a) = [a]
    | digitToList (Two (a, b)) = [a, b]
    | digitToList (Three (a, b, c)) = [a, b, c]
    | digitToList (Four (a, b, c, d)) = [a, b, c, d]

  fun nodeToDigit (Node2 (_, a, b)) = Two (a, b)
    | nodeToDigit (Node3 (_, a, b, c)) = Three (a, b, c)
    | nodeToDigit (Leaf _) = raise Fail "fingertree: leaf in spine middle"

  fun digitToTree (One a) = Single a
    | digitToTree (Two (a, b)) = mkDeep (One a, Empty, One b)
    | digitToTree (Three (a, b, c)) = mkDeep (Two (a, b), Empty, One c)
    | digitToTree (Four (a, b, c, d)) = mkDeep (Two (a, b), Empty, Two (c, d))

  (* ---- cons / snoc (node level) ---- *)
  fun consN (a, Empty) = Single a
    | consN (a, Single b) = mkDeep (One a, Empty, One b)
    | consN (a, Deep (_, One b, m, sf)) = mkDeep (Two (a, b), m, sf)
    | consN (a, Deep (_, Two (b, c), m, sf)) = mkDeep (Three (a, b, c), m, sf)
    | consN (a, Deep (_, Three (b, c, d), m, sf)) = mkDeep (Four (a, b, c, d), m, sf)
    | consN (a, Deep (_, Four (b, c, d, e), m, sf)) =
        mkDeep (Two (a, b), consN (mkNode3 (c, d, e), m), sf)

  fun snocN (Empty, a) = Single a
    | snocN (Single b, a) = mkDeep (One b, Empty, One a)
    | snocN (Deep (_, pr, m, One b), a) = mkDeep (pr, m, Two (b, a))
    | snocN (Deep (_, pr, m, Two (b, c)), a) = mkDeep (pr, m, Three (b, c, a))
    | snocN (Deep (_, pr, m, Three (b, c, d)), a) = mkDeep (pr, m, Four (b, c, d, a))
    | snocN (Deep (_, pr, m, Four (b, c, d, e)), a) =
        mkDeep (pr, snocN (m, mkNode3 (b, c, d)), Two (e, a))

  fun cons x t = consN (Leaf x, t)
  fun snoc x t = snocN (t, Leaf x)

  (* ---- views (node level) ---- *)
  fun lheadDigit (One a) = (a, NONE)
    | lheadDigit (Two (a, b)) = (a, SOME (One b))
    | lheadDigit (Three (a, b, c)) = (a, SOME (Two (b, c)))
    | lheadDigit (Four (a, b, c, d)) = (a, SOME (Three (b, c, d)))

  fun rheadDigit (One a) = (a, NONE)
    | rheadDigit (Two (a, b)) = (b, SOME (One a))
    | rheadDigit (Three (a, b, c)) = (c, SOME (Two (a, b)))
    | rheadDigit (Four (a, b, c, d)) = (d, SOME (Three (a, b, c)))

  fun viewLN Empty = NONE
    | viewLN (Single x) = SOME (x, Empty)
    | viewLN (Deep (_, pr, m, sf)) =
        let val (a, pr') = lheadDigit pr
        in case pr' of
               SOME d => SOME (a, mkDeep (d, m, sf))
             | NONE => SOME (a, deepL (m, sf))
        end
  and deepL (m, sf) =
        case viewLN m of
            NONE => digitToTree sf
          | SOME (node, m') => mkDeep (nodeToDigit node, m', sf)

  fun viewRN Empty = NONE
    | viewRN (Single x) = SOME (x, Empty)
    | viewRN (Deep (_, pr, m, sf)) =
        let val (a, sf') = rheadDigit sf
        in case sf' of
               SOME d => SOME (a, mkDeep (pr, m, d))
             | NONE => SOME (a, deepR (pr, m))
        end
  and deepR (pr, m) =
        case viewRN m of
            NONE => digitToTree pr
          | SOME (node, m') => mkDeep (pr, m', nodeToDigit node)

  fun viewL t =
    case viewLN t of NONE => NONE | SOME (x, t') => SOME (unLeaf x, t')
  fun viewR t =
    case viewRN t of NONE => NONE | SOME (x, t') => SOME (unLeaf x, t')

  (* the spine constructor `Empty` shadows the basis exception; List.Empty is it *)
  fun head t = case viewL t of SOME (x, _) => x | NONE => raise List.Empty
  fun last t = case viewR t of SOME (x, _) => x | NONE => raise List.Empty

  (* ---- append ---- *)
  fun nodes [a, b] = [mkNode2 (a, b)]
    | nodes [a, b, c] = [mkNode3 (a, b, c)]
    | nodes [a, b, c, d] = [mkNode2 (a, b), mkNode2 (c, d)]
    | nodes (a :: b :: c :: rest) = mkNode3 (a, b, c) :: nodes rest
    | nodes _ = raise Fail "fingertree: nodes arity"

  fun app3 (Empty, ts, r) = List.foldr consN r ts
    | app3 (l, ts, Empty) = List.foldl (fn (a, acc) => snocN (acc, a)) l ts
    | app3 (Single x, ts, r) = consN (x, List.foldr consN r ts)
    | app3 (l, ts, Single x) = snocN (List.foldl (fn (a, acc) => snocN (acc, a)) l ts, x)
    | app3 (Deep (_, pr1, m1, sf1), ts, Deep (_, pr2, m2, sf2)) =
        let
          val mid = nodes (digitToList sf1 @ ts @ digitToList pr2)
        in mkDeep (pr1, app3 (m1, mid, m2), sf2) end

  fun append a b = app3 (a, [], b)

  (* ---- positional lookup ---- *)
  fun lookupNode (Leaf a, _) = a
    | lookupNode (Node2 (_, a, b), i) =
        let val sa = nsize a
        in if i < sa then lookupNode (a, i) else lookupNode (b, i - sa) end
    | lookupNode (Node3 (_, a, b, c), i) =
        let val sa = nsize a val sb = nsize b
        in if i < sa then lookupNode (a, i)
           else if i < sa + sb then lookupNode (b, i - sa)
           else lookupNode (c, i - sa - sb)
        end

  fun lookupDigit (d, i) =
    let
      fun go ([], _) = raise Subscript
        | go (n :: ns, j) =
            let val sn = nsize n
            in if j < sn then lookupNode (n, j) else go (ns, j - sn) end
    in go (digitToList d, i) end

  fun lookupFT (Empty, _) = raise Subscript
    | lookupFT (Single x, i) = lookupNode (x, i)
    | lookupFT (Deep (_, pr, m, sf), i) =
        let val spr = dsize pr
        in if i < spr then lookupDigit (pr, i)
           else
             let val sm = ftsize m
             in if i < spr + sm then lookupFT (m, i - spr)
                else lookupDigit (sf, i - spr - sm)
             end
        end

  fun index t i =
    if i < 0 orelse i >= ftsize t then raise Subscript
    else lookupFT (t, i)

  (* ---- split (peel from the front; preserves order, persistent) ---- *)
  fun splitAt t i =
    let
      val n = ftsize t
      val k = if i < 0 then 0 else if i > n then n else i
      fun go (0, left, right) = (left, right)
        | go (j, left, right) =
            (case viewL right of
                 NONE => (left, right)
               | SOME (x, right') => go (j - 1, snoc x left, right'))
    in go (k, empty, t) end

  (* ---- conversions ---- *)
  fun fromList xs = List.foldr (fn (x, t) => cons x t) empty xs

  fun toList t =
    case viewL t of NONE => [] | SOME (x, t') => x :: toList t'
end
