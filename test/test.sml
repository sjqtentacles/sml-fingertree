(* Tests for sml-fingertree: 2-3 finger tree as a persistent sequence / deque.

   The finger tree is cross-checked against the obvious list model: `toList` of a
   built tree must equal the list it was built from, `index` must agree with
   `List.nth`, `append` must equal list append (and be associative), and the
   deque operations (`cons`/`snoc`/`viewL`/`viewR`) must mirror front/back list
   operations. Large 200-element round-trips exercise the deep spine. *)

structure Tests =
struct
  open Harness

  structure F = Fingertree

  fun fromTo a b = List.tabulate (b - a + 1, fn i => a + i)
  val n200 = fromTo 1 200
  (* a scrambled permutation of 1..200 (stride 73, coprime to 200) *)
  val scram = List.tabulate (200, fn i => (i * 73) mod 200 + 1)

  fun runAll () =
    let
      val () = section "Empty"
      val e : int F.t = F.empty
      val () = checkBool "isEmpty" (true, F.isEmpty e)
      val () = checkInt "length 0" (0, F.length e)
      val () = checkBool "viewL NONE" (true, not (Option.isSome (F.viewL e)))
      val () = checkBool "viewR NONE" (true, not (Option.isSome (F.viewR e)))
      val () = checkIntList "toList []" ([], F.toList e)
      val () = checkBool "fromList [] empty" (true, F.isEmpty (F.fromList ([] : int list)))

      val () = section "cons / snoc ordering"
      val c3 = F.cons 1 (F.cons 2 (F.cons 3 F.empty))
      val () = checkIntList "cons builds front-first" ([1,2,3], F.toList c3)
      val s3 = F.snoc 3 (F.snoc 2 (F.snoc 1 F.empty))
      val () = checkIntList "snoc builds back-last" ([1,2,3], F.toList s3)
      val () = checkInt "length after cons" (3, F.length c3)
      val mix = F.snoc 3 (F.cons 0 (F.snoc 2 (F.cons 1 F.empty)))
      val () = checkIntList "interleaved cons/snoc" ([0,1,2,3], F.toList mix)

      val () = section "fromList / toList round trip"
      val t = F.fromList n200
      val () = checkInt "length 200" (200, F.length t)
      val () = checkIntList "round trip 1..200" (n200, F.toList t)
      val ts = F.fromList scram
      val () = checkIntList "round trip scrambled" (scram, F.toList ts)
      val () = checkInt "scrambled length" (200, F.length ts)

      val () = section "head / last / viewL / viewR"
      val () = checkInt "head" (1, F.head t)
      val () = checkInt "last" (200, F.last t)
      val () = checkBool "viewL splits head"
                 (true, case F.viewL t of SOME (x, r) => x = 1 andalso F.toList r = fromTo 2 200 | NONE => false)
      val () = checkBool "viewR splits last"
                 (true, case F.viewR t of SOME (x, r) => x = 200 andalso F.toList r = fromTo 1 199 | NONE => false)
      val () = checkRaises "head empty raises" (fn () => F.head F.empty)
      val () = checkRaises "last empty raises" (fn () => F.last F.empty)

      val () = section "viewL drains in order"
      val drained =
        let fun go (acc, ft) = case F.viewL ft of NONE => List.rev acc
                                                 | SOME (x, ft') => go (x :: acc, ft')
        in go ([], ts) end
      val () = checkIntList "viewL full drain = scrambled" (scram, drained)
      val drainedR =
        let fun go (acc, ft) = case F.viewR ft of NONE => acc
                                                | SOME (x, ft') => go (x :: acc, ft')
        in go ([], ts) end
      val () = checkIntList "viewR full drain = scrambled" (scram, drainedR)

      val () = section "index matches List.nth"
      val () = checkBool "every index (1..200)"
                 (true, List.all (fn i => F.index t i = List.nth (n200, i))
                                 (List.tabulate (200, fn i => i)))
      val () = checkBool "every index (scrambled)"
                 (true, List.all (fn i => F.index ts i = List.nth (scram, i))
                                 (List.tabulate (200, fn i => i)))
      val () = checkInt "index 0" (1, F.index t 0)
      val () = checkInt "index 199" (200, F.index t 199)
      val () = checkRaises "index -1 raises" (fn () => F.index t ~1)
      val () = checkRaises "index 200 raises" (fn () => F.index t 200)

      val () = section "append vs list append"
      val la = fromTo 1 30 and lb = fromTo 31 75 and lc = fromTo 76 200
      val fa = F.fromList la and fb = F.fromList lb and fc = F.fromList lc
      val () = checkIntList "append a b" (la @ lb, F.toList (F.append fa fb))
      val () = checkInt "append length" (200, F.length (F.append (F.append fa fb) fc))
      val () = checkIntList "left assoc" (la @ lb @ lc, F.toList (F.append (F.append fa fb) fc))
      val () = checkIntList "right assoc" (la @ lb @ lc, F.toList (F.append fa (F.append fb fc)))
      val () = checkIntList "append empty L" (la, F.toList (F.append F.empty fa))
      val () = checkIntList "append empty R" (la, F.toList (F.append fa F.empty))
      val () = checkBool "associativity"
                 (true, F.toList (F.append (F.append fa fb) fc)
                        = F.toList (F.append fa (F.append fb fc)))

      val () = section "splitAt"
      val () = checkBool "split at every boundary recombines + halves match list"
                 (true,
                  List.all
                    (fn i =>
                       let val (l, r) = F.splitAt t i
                       in F.toList l = List.take (n200, i)
                          andalso F.toList r = List.drop (n200, i)
                          andalso F.toList (F.append l r) = n200
                       end)
                    (List.tabulate (201, fn i => i)))
      val (l50, r50) = F.splitAt t 50
      val () = checkIntList "split left 50" (fromTo 1 50, F.toList l50)
      val () = checkIntList "split right 150" (fromTo 51 200, F.toList r50)
      val () = checkInt "split lengths sum" (200, F.length l50 + F.length r50)
      val (lz, rz) = F.splitAt t 0
      val () = checkBool "split at 0 -> empty left" (true, F.isEmpty lz)
      val () = checkIntList "split at 0 -> full right" (n200, F.toList rz)

      val () = section "deque behaviour"
      val d = F.snoc 4 (F.snoc 3 (F.cons 1 (F.cons 0 (F.fromList [2]))))
      (* cons 0, cons 1 onto [2] -> [1,0,2]? careful: cons prepends *)
      (* fromList [2] = [2]; cons 0 -> [0,2]; cons 1 -> [1,0,2]; snoc 3 -> [1,0,2,3]; snoc 4 -> [1,0,2,3,4] *)
      val () = checkIntList "deque assembled" ([1,0,2,3,4], F.toList d)
      val () = checkInt "deque head" (1, F.head d)
      val () = checkInt "deque last" (4, F.last d)
      val () = checkBool "pop front then back"
                 (true,
                  case F.viewL d of
                      SOME (x, d1) =>
                        (case F.viewR d1 of
                             SOME (y, d2) => x = 1 andalso y = 4 andalso F.toList d2 = [0,2,3]
                           | NONE => false)
                    | NONE => false)

      val () = section "persistence"
      val base = F.fromList [1,2,3]
      val b2 = F.cons 0 base
      val b3 = F.snoc 4 base
      val () = checkIntList "original unchanged" ([1,2,3], F.toList base)
      val () = checkIntList "cons branch" ([0,1,2,3], F.toList b2)
      val () = checkIntList "snoc branch" ([1,2,3,4], F.toList b3)
    in
      Harness.run ()
    end

  val run = runAll
end
