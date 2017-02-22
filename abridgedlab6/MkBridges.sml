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
      fun DFS (u, v, cnt, low, pre, bridges) = 
        let
          val newCnt = cnt + 1
          val newPre = STSeq.update (v, cnt) pre
          val newLow = STSeq.update (v, cnt) low
          val neighbors = nth G v 
          fun loop ((C, L, P, B), w) = 
            if (STSeq.nth P w = ~1) then let
              val (nC, nL, nP, nB) = DFS (v, w, C, L, P, B)
              val getL = STSeq.update (v, Int.min(STSeq.nth nL v, STSeq.nth nL w)) nL
              val getB = if (STSeq.nth getL w = STSeq.nth nP w) then append (nB, singleton(v, w)) else nB
            in
              (nC, getL, nP, getB)
            end
            else if (w <> u) then let
              val getL = STSeq.update (v, Int.min(STSeq.nth L v, STSeq.nth P w)) L
            in
              (C, getL, P, B)
            end
            else (C, L, P, B)
        in
          iter loop (newCnt, newLow, newPre, bridges) (nth G v)
        end
      fun API ((c, l, p, b), n) = DFS (n, n, c, l, p, b)
      val initL = STSeq.fromSeq (tabulate (fn _ => ~1) (length G))
      val initP = STSeq.fromSeq (tabulate (fn _ => ~1) (length G))
      val allNode = tabulate (fn i => i) (length G)
    in
      #4 (iter API (0, initL, initP, empty()) allNode)
    end

end
