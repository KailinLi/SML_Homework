functor MkSeqUtil(structure S : SEQUENCE) : SEQUENCE_UTIL =
struct
  structure Seq = S
  open Seq

  type 'a hist = ('a * int) seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  fun tokens (cp : char -> bool) (str : string) : string seq =
    let
      val n = String.size str
      val chars = tabulate (fn i => (i, String.sub (str, i))) n
      val idx = map (fn (i,_) => i) (filter (fn (_,c) => cp c) chars)

      (* grab substrings in between delimiters *)
      val subs = map2 (fn (i,i') => String.substring (str, i, i' - i))
                      (append (singleton 0, map (fn i => i + 1) idx))
                      (append (idx, singleton n))
    in filter (fn s => size s > 0) subs
    end

  fun histogram (cmp : 'a ord) (s : 'a seq) : 'a hist =
    map (fn (a, c) => (a, length c))
        (collect cmp (map (fn a => (a, ())) s))

  fun choose (hist : 'a hist) (p : real) : 'a =
    if p > 1.0 orelse p < 0.0 then raise Range
    else let
        val id = #1 (nth hist 0)
        fun add ((_, num1), (title, num2)) = (title, num1 + num2)
        val tmp = scani add (id, 0) hist
        val atHist = Real.ceil (p * Real.fromInt (#2 (nth tmp (length tmp - 1))))
    in
      #1 (reduce (fn ((title1, num1), (title2, num2)) => if num1 < atHist then (title2, num2) else (title1, num1)) (id, 0) tmp)
    end
end
