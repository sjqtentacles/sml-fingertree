# sml-fingertree

[![CI](https://github.com/sjqtentacles/sml-fingertree/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-fingertree/actions/workflows/ci.yml)

2-3 **finger trees** (Hinze & Paterson) giving a general-purpose persistent
sequence / deque, in pure Standard ML.

A finger tree supports amortised **O(1)** access and update at both ends
(`cons`, `snoc`, `viewL`, `viewR`), **O(log n)** concatenation (`append`),
positional lookup (`index`), and split (`splitAt`), via a cached size measure at
every node. Every operation is **persistent**: it returns a new tree and never
mutates its argument, so old versions stay valid.

No dependencies, no FFI, no threads, no clock, no randomness: the same inputs
always produce the same outputs under **MLton** and **Poly/ML**.

> Implementation note: to stay within Standard ML's monomorphic recursion,
> elements are wrapped in a single uniform `node` type (`Leaf` at the bottom,
> `Node2`/`Node3` deeper) so the spine type `'a t` is *not* nested. The level
> invariant is maintained internally; sizes are cached at every node and `Deep`.

## API

```sml
structure Fingertree : sig
  type 'a t
  val empty   : 'a t
  val isEmpty : 'a t -> bool
  val length  : 'a t -> int
  val cons    : 'a -> 'a t -> 'a t              (* push onto the front *)
  val snoc    : 'a -> 'a t -> 'a t              (* push onto the back *)
  val viewL   : 'a t -> ('a * 'a t) option      (* uncons from the front *)
  val viewR   : 'a t -> ('a * 'a t) option      (* uncons from the back *)
  val head    : 'a t -> 'a                       (* raises Empty if empty *)
  val last    : 'a t -> 'a                       (* raises Empty if empty *)
  val append  : 'a t -> 'a t -> 'a t
  val index   : 'a t -> int -> 'a               (* 0-based; raises Subscript *)
  val splitAt : 'a t -> int -> 'a t * 'a t      (* first i elements, rest *)
  val fromList : 'a list -> 'a t
  val toList   : 'a t -> 'a list
end
```

## Example

```sml
val t = Fingertree.fromList [1,2,3,4,5]
val SOME (1, _) = Fingertree.viewL t        (* front *)
val SOME (5, _) = Fingertree.viewR t        (* back *)
val t2 = Fingertree.cons 0 (Fingertree.snoc 6 t)
val [0,1,2,3,4,5,6] = Fingertree.toList t2
val 3 = Fingertree.index t2 3               (* O(log n) positional lookup *)
val (l, r) = Fingertree.splitAt t2 3        (* ([0,1,2], [3,4,5,6]) *)
val ab = Fingertree.append (Fingertree.fromList [1,2]) (Fingertree.fromList [3,4])
```

Running [`examples/demo.sml`](examples/demo.sml) with `make example` prints:

```
Build a deque: cons 0, cons 1 onto [2,3], then snoc 4, snoc 5:
  toList   = [1,0,2,3,4,5]
  length   = 6
  head/last= 1 / 5

Pop from both ends:
  viewL -> 1, rest [0,2,3,4,5]
  viewR -> 5, rest [0,2,3,4]

Indexed sequence over 1..20:
  index 0  = 1
  index 9  = 10
  index 19 = 20
  splitAt 7 left  = [1,2,3,4,5,6,7]
  splitAt 7 right = [8,9,10,11,12,13,14,15,16,17,18,19,20]

Append [1,2,3] ++ [4,5,6]:
  toList   = [1,2,3,4,5,6]
```

## Build & test

Requires [MLton](http://mlton.org/) and/or [Poly/ML](https://polyml.org/).

```sh
make test        # build + run the suite under MLton
make test-poly   # run the suite under Poly/ML
make all-tests   # both
make example     # build + run the demo
make clean
```

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-fingertree
smlpkg sync
```

Reference `lib/github.com/sjqtentacles/sml-fingertree/fingertree.mlb` from your
own `.mlb` (MLton / MLKit), or feed `sources.mlb` to `tools/polybuild` (Poly/ML).

## Layout

```
sml.pkg                                       smlpkg manifest
Makefile                                      MLton + Poly/ML targets
.github/workflows/ci.yml                      CI: MLton + Poly/ML
lib/github.com/sjqtentacles/sml-fingertree/
  fingertree.sig   FINGERTREE signature
  fingertree.sml   2-3 finger tree with size measure
  sources.mlb      ordered source list
  fingertree.mlb   public basis
examples/
  demo.sml         deque + indexed-sequence walkthrough
test/
  harness.sml      shared assertion harness
  test.sml         deque / index / append / split vectors (48 checks)
  entry.sml / main.sml
tools/polybuild    Poly/ML build wrapper
```

## Tests

48 deterministic checks, cross-checked against the obvious list model. `toList`
of a tree equals the list it was built from (sequential `1..200` and a scrambled
permutation); `index` agrees with `List.nth` at every position; `append` equals
list append and is associative; `splitAt` at every boundary `0..200` matches
`List.take`/`List.drop` and recombines to the original; and the deque operations
(`cons`/`snoc`/`viewL`/`viewR`/`head`/`last`) mirror front/back list operations,
with persistence checked (originals never mutate). Run `make all-tests` to verify
identical output under both compilers.

## License

MIT. See [LICENSE](LICENSE).
