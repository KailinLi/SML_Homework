functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  (* You must define the following two types and
   * explain your decision here with comments.
   *)

  (*一个table，然后每个点作为key值，映射到一个邻居节点的集合*)
  type graph = Set.set table

  (*一个table，每个key对应要到达的顶点，一个vertex类型的串表示路径*)
  type asp = vertex seq table

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
    Table.map Set.fromSeq (Table.collect E)

  (* Task 2.2 *)
  fun numEdges (G : graph) : int =
    let
      val count = Table.map Set.size G
      fun add (x, y) = x + y
    in
      Table.reduce add 0 count
    end

  fun numVertices (G : graph) : int =
    let
      val endPoints = Table.range G
      val allEnd = Seq.flatten (Seq.map Set.toSeq endPoints)
    in
      Set.size (Set.union (Table.domain G, Set.fromSeq allEnd))
    end

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    case Table.find G v of
       NONE => Seq.empty()
     | SOME (answer) => Set.toSeq answer

       (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp = let
    fun BFS (pathTable : asp) (frontier : vertex seq) : asp =
      if (Seq.length frontier) = 0 then pathTable
      else let
        val visitingVertexs = Table.collect (Seq.flatten (
          Seq.map
            (fn u => Seq.map (fn v => (v, u))
            (outNeighbors G u))
            frontier
        ))
        (*每一个key指向其之前的元素*)
        val newPath = Table.merge (fn (sqPar, _) => sqPar)
                           (pathTable, visitingVertexs)
        val newFrontier = Set.toSeq (Table.domain (
          Table.erase (visitingVertexs, Table.domain pathTable)
        ))
        (*删除访问过的节点*)
      in
        BFS newPath newFrontier
      end
    in
      BFS (Table.singleton (v, Seq.empty ())) (Seq.singleton v)
    end

  (* Task 2.5 *)
  fun report (A : asp) (v : vertex) : vertex seq seq =
  let
    fun findList (v: vertex) : vertex seq seq =
      case Table.find A v of
         NONE => Seq.empty()
       | SOME(points) => if Seq.length points = 0 then 
                                  Seq.singleton (Seq.singleton v)
                          else Seq.map (fn path => 
                                          Seq.append(path, Seq.singleton v)) (
                            Seq.flatten (Seq.map (fn p => findList p)points)
                          )
  in
    findList v
  end

end
