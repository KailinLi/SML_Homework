functor MkBoruvkaSegmenter
  (structure Seq : SEQUENCE
   structure Rand : RANDOM210
   sharing Seq = Rand.Seq)
  : SEGMENTER =
struct
  structure Seq = Rand.Seq
  open Seq

  structure R = Rand
  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight


(*和上一个原理基本相同*)
  fun findSegments (E, n) initial_credit =
    let
    val sorted = rev (sort (fn ((s1,d1,w1),(s2,d2,w2))=>Int.compare (w1,w2)) E)
    val verts = tabulate (fn i => i) n
    val creds = tabulate (fn _ => initial_credit) n
    fun helper (Vs,Es,C) s =
      let
        val coins = Rand.flip s n
        fun ifContract (s,(d,w)) = (nth coins s = 0) andalso (nth coins d = 1)
        val minEs = filter (fn (_,(a,_))=>a >= 0) (enum (inject Es (tabulate (fn _ => (~1,~1)) n)))
        val contract = filter ifContract minEs
        val recover = map (fn (s,(d,_))=>(s,d)) contract
        val P = inject recover Vs
        val updUF = map (fn v => nth P v) P
        val revConts = collect Int.compare (map (fn (s,(d,w))=>(d,(s,w))) contract)
        val deltas = map (fn (d, p) => (d, reduce op+ 0 (map (fn (_,w)=>w) p))) revConts
        val minCreds = map (fn (d, p) => (d, (Int.min(nth C d, reduce Int.min initial_credit (map (fn (s,w)=>nth C s) p))))) revConts
        val updCreds = inject minCreds C
        val finalCreds = inject (map (fn (d, delt)=>(d, nth updCreds d - delt)) deltas) updCreds 
        val E' = map (fn (s, (d, w)) => ((nth updUF s),(nth updUF d, w))) Es
        val updE = filter (fn (s, (d, _))=> s <> d) E'
        val finalE = filter (fn (s, (d, w))=> w < nth finalCreds s andalso w < nth finalCreds d) updE
      in 
        if (length Es = 0) then updUF else helper (updUF, finalE, finalCreds) (Rand.next s)
      end
      val res = helper (verts, (map (fn (s, d, w) => (s, (d, w))) sorted), creds) (Rand.fromInt 1028)
    in
      map (fn v => nth res v) res
    end 
end