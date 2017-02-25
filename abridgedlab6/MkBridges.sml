functor MkBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  open Seq

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq


  type ugraph = vertex seq seq

(*用seq每一个序号对应起点序号*)
  fun makeGraph (E : edge seq) : ugraph =
    let
      val reverse = Seq.map (fn (a, b) => (b, a)) E 
      val all = Seq.append (E, reverse)
      val collectVertex = Seq.collect Int.compare all
    in
      Seq.map (fn (_, vertexs) => vertexs) collectVertex
    end


(*改编自Tarjan的一个python算法，纯粹自己写，考虑细节实在是太多了*)
  fun findBridges (G : ugraph) : edges =
    let
      fun DFS (u, v, cnt, low, dfn, bridges) = 
        let
          val newCnt = cnt + 1
          val newDfn = STSeq.update (v, cnt) dfn
          val newLow = STSeq.update (v, cnt) low
          val neighbors = nth G v 
          fun loop ((C, L, D, B), w) = 
            if (STSeq.nth D w = ~1) then let
              val (nC, nL, nD, nB) = DFS (v, w, C, L, D, B)
              val getL = STSeq.update (v, Int.min(STSeq.nth nL v, STSeq.nth nL w)) nL
              val getB = if (STSeq.nth getL w = STSeq.nth nD w) then append (nB, singleton(v, w)) else nB
            in
              (nC, getL, nD, getB)
            end
            else if (w <> u) then let
              val getL = STSeq.update (v, Int.min(STSeq.nth L v, STSeq.nth D w)) L
            in
              (C, getL, D, B)
            end
            else (C, L, D, B)
        in
          iter loop (newCnt, newLow, newDfn, bridges) (nth G v)
        end
      fun API ((c, l, d, b), n) = DFS (n, n, c, l, d, b)
      val initL = STSeq.fromSeq (tabulate (fn _ => ~1) (length G))
      val initD = STSeq.fromSeq (tabulate (fn _ => ~1) (length G))
      val allNode = tabulate (fn i => i) (length G)
    in
      #4 (iter API (0, initL, initD, empty()) allNode)
    end

end
