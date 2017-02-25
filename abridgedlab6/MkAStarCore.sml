functor MkAStarCore(structure Table : TABLE
                    structure PQ : PQUEUE
                      where type Key.t = real) : ASTAR =
struct
  structure Set = Table.Set
  structure Seq = Set.Seq

  type weight = real
  type vertex = Set.key
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real


  (* Define this type yourself *)
  (*因为连通有向图，不一定每个点都有对应的儿子节点，所以采用table比较方便，时间复杂度也能控制*)
  type graph = weight Table.table Table.table

  fun makeGraph (E : edge Seq.seq) : graph =
    let
      val result = Table.collect (Seq.map (fn (u, v, w) => (u, (v, w))) E)
    in
      Table.map Table.fromSeq result
    end

  fun findPath h G (S, T) =
    let
        fun N(v) =
            case Table.find G v
              of NONE => Table.empty ()
               | SOME nbr => nbr
        fun Dijkstra D Q =
            case PQ.deleteMin Q
              of (NONE, _) => D
               | (SOME (d, v), Q') =>
                 case Table.find D v
                   of SOME _ => Dijkstra D Q'
                    | NONE =>
                      let
                        val insert = Table.insert (fn (x, _) => x)
                        val D' = insert (v, d - (h v)) D
                        fun relax (q, (u, w)) = PQ.insert (d + w - (h v) + (h u), u) q
                        val Q'' = Table.iter relax Q' (N v)
                      in if (Set.find T v) then D' else Dijkstra D' Q''
                      end
        val init = PQ.fromList (Seq.toList (Seq.map (fn p => ((h p), p)) (Set.toSeq S)))
        val answer = Dijkstra (Table.empty ()) init
        val final = Table.toSeq (Table.extract (answer, T))
        (*仅仅用来调整函数类型，适应Dijkstra*)
      in
        if (Seq.length final = 0) then NONE else SOME (Seq.nth final 0)
        (*第一个是最小的*)
      end

end
