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

(*def bridge_dfs(G,u,v,cnt,low,pre,bridges):
    cnt    += 1
    pre[v]  = cnt
    low[v]  = pre[v]

    for w in nx.neighbors(G,v):
        if (pre[w] == -1):
            bridge_dfs(G,v,w,cnt,low,pre,bridges)

            low[v] = min(low[v], low[w])
            if (low[w] == pre[w]):
                bridges.append((v,w))

        elif (w != u):
            low[v] = min(low[v], pre[w])

def get_bridges(G):
    bridges = []
    cnt     = 0
    low     = {n:-1 for n in G.nodes()}
    pre     = low.copy()

    for n in G.nodes():
         bridge_dfs(G, n, n, cnt, low, pre, bridges)

    return bridges # <- List of (node-node) tuples for all bridges in G*)


  fun findBridges (G : ugraph) : edges =
    let
      fun DFS (u, v, cnt, low, pre, bridges) = 
        let
          val newCnt = cnt + 1
          fun change (i, key) = if i = v then cnt else key
          val newPre = mapIdx change pre 
          val newLow = mapIdx change low
          val neighbors = nth G v 
          fun loop ((C, L, P, B), w) = 
            if (nth P w = ~1) then let
              val (nC, nL, nP, nB) = DFS (v, w, C, L, P, B)
              val getL = mapIdx (fn (i, key) => if i = v then Int.min(nth nL v, nth nL w) else key) nL 
              val getB = if (nth getL w = nth nP w) then append (nB, singleton(v, w)) else nB
            in
              (nC, getL, nP, getB)
            end
            else if (w <> u) then let
              val getL = mapIdx (fn (i, key) => if i = v then Int.min(nth L v, nth P w) else key) L
            in
              (C, getL, P, B)
            end
            else (C, L, P, B)
        in
          iter loop (newCnt, newLow, newPre, bridges) (nth G v)
        end
      fun API ((c, l, p, b), n) = DFS (n, n, c, l, p, b)
      val initL = tabulate (fn _ => ~1) (length G)
      val initP = tabulate (fn _ => ~1) (length G)
      val allNode = tabulate (fn i => i) (length G)
    in
      #4 (iter API (0, initL, initP, empty()) allNode)
    end

end
