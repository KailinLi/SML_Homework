functor MkBoruvkaMST (structure Seq : SEQUENCE
                      structure Rand : RANDOM210
                      sharing Seq = Rand.Seq) : MST =
struct
  structure Seq = Rand.Seq
  open Seq

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight


  fun MST (E : edge seq, n : int) : edge seq =
    let
      val sortByWight = Seq.sort (fn ((u1, v1, w1), (u2, v2, w2)) => Int.compare (w2, w1)) E
      val edges = Seq.map (fn (u, v, w) => (u, (v, (u, v, w)))) sortByWight
      val vertexs = tabulate (fn i => i) n 
      fun boruvka (Vs, Es, T, Seed) = 
        let
          val coins = Rand.flip Seed n 
          fun ifContract (u, (v, point)) = (nth coins u = 0) andalso (nth coins v = 1)
          (*随机抛硬币*)
          val minE = filter (fn (_, (v, _)) => v >= 0) (enum (inject Es (tabulate (fn _ => (~1, (~1, ~1, ~1))) n)))
          val contract = filter ifContract minE
          val recover = map (fn (u, (v, _)) => (u, v)) contract
          val P = inject recover Vs
          val newT = append ((map (fn (_, (_, point)) => point) contract), T)
          val newE = map (fn (u, (v, point)) => ((nth P u), (nth P v, point))) Es 
          val updata = filter (fn (u, (v, _)) => u <> v) newE
        in
          if (length updata = 0) then newT else boruvka (P, updata, newT, Rand.next Seed)
        end
    in
      boruvka (vertexs, edges, empty(), Rand.fromInt 1028)
    end 


end
