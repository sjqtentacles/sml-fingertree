(* demo.sml - finger tree as a persistent deque / indexed sequence.
   Deterministic: identical output on every run and under both compilers. *)

structure F = Fingertree

fun intList xs = "[" ^ String.concatWith "," (List.map Int.toString xs) ^ "]"

val () = print "Build a deque: cons 0, cons 1 onto [2,3], then snoc 4, snoc 5:\n"
val d0 = F.fromList [2, 3]
val d = F.snoc 5 (F.snoc 4 (F.cons 1 (F.cons 0 d0)))
val () = print ("  toList   = " ^ intList (F.toList d) ^ "\n")
val () = print ("  length   = " ^ Int.toString (F.length d) ^ "\n")
val () = print ("  head/last= " ^ Int.toString (F.head d) ^ " / "
                ^ Int.toString (F.last d) ^ "\n")

val () = print "\nPop from both ends:\n"
val () =
  case F.viewL d of
      SOME (x, d1) =>
        (print ("  viewL -> " ^ Int.toString x ^ ", rest " ^ intList (F.toList d1) ^ "\n");
         case F.viewR d1 of
             SOME (y, d2) =>
               print ("  viewR -> " ^ Int.toString y ^ ", rest " ^ intList (F.toList d2) ^ "\n")
           | NONE => ())
    | NONE => ()

val () = print "\nIndexed sequence over 1..20:\n"
val t = F.fromList (List.tabulate (20, fn i => i + 1))
val () = print ("  index 0  = " ^ Int.toString (F.index t 0) ^ "\n")
val () = print ("  index 9  = " ^ Int.toString (F.index t 9) ^ "\n")
val () = print ("  index 19 = " ^ Int.toString (F.index t 19) ^ "\n")
val (l, r) = F.splitAt t 7
val () = print ("  splitAt 7 left  = " ^ intList (F.toList l) ^ "\n")
val () = print ("  splitAt 7 right = " ^ intList (F.toList r) ^ "\n")

val () = print "\nAppend [1,2,3] ++ [4,5,6]:\n"
val ab = F.append (F.fromList [1,2,3]) (F.fromList [4,5,6])
val () = print ("  toList   = " ^ intList (F.toList ab) ^ "\n")
