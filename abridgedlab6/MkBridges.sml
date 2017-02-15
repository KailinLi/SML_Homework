functor MkBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  open Seq

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  (* Remove these two lines before submittting. *)
  exception NotYetImplemented

  type ugraph = vertex seq seq

  fun makeGraph (E : edge seq) : ugraph =
    let
      val reverse = Seq.map (fn (a, b) => (b, a)) E 
      val all = Seq.append (E, reverse)
      val collectVertex = Seq.collect Int.compare all
    in
      Seq.map (fn (_, vertexs) => vertexs) collectVertex
    end


    (*let
        fun DFS p ((B,X,c,m),v) =
        if (isSome(STSeq.nth X v)) then (B,X,c,Int.min(m,valOf (STSeq.nth X v)))
        else let
            val updX = STSeq.update (v,SOME c) X
            val toVisit = filter (fn v=> v<> p) (nth G v)
            val (fixB,fixX,fixc,fixm) = iter (DFS v) (B,updX,c+1,(length G)) toVisit
            val finalB = if p<>v andalso fixm >= c then (STSeq.update (v,singleton((p,v))) fixB) else fixB
            in (finalB,fixX,fixc,Int.min(m,fixm)) end
       val Vert = tabulate (fn i=>i) (length G)
       val X = STSeq.fromSeq (tabulate (fn _=>NONE) (length G))
       val B = STSeq.fromSeq (tabulate (fn _=>empty()) (length G))
       val res =  #1(iter (fn(S,v)=> DFS v (S,v)) (B,X,0,0) Vert)
      in
      flatten(STSeq.toSeq res)
end*)


  fun findBridges (G : ugraph) : edges =
    let
      fun DFS p ((B, X, c, m), v) =
        if (isSome(STSeq.nth X v)) then (B, X, c, Int.min(m, valOf(STSeq.nth X v)))
          else
            let
              val updX = STSeq.update (v, SOME c) X
              val toVisit = filter (fn v => v <> p) (nth G v)
              val (fixB,fixX,fixc,fixm) = iter (DFS v) (B, updX, c + 1, (length G)) toVisit
              val finalB = if p <> v andalso fixm >= c then (STSeq.update (v, singleton (p, v)) fixB) else fixB
            in
              (finalB, fixX, fixc, Int.min(m, fixm))
            end
          val Vert = tabulate (fn i => i) (length G)
          val X = STSeq.fromSeq (tabulate (fn _ => NONE) (length G))
          val B = STSeq.fromSeq (tabulate (fn _ => empty()) (length G))
          val res = #1 (iter (fn (S, v) => DFS v (S, v)) (B, X, 0, 0) Vert)
    in
      flatten (STSeq.toSeq res)
    end

end
