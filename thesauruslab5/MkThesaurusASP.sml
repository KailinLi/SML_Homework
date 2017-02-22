functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq

  (*和前面的题目类比，numWord就是节点个数，synonyms就是邻居，可以直接类比*)
  type thesaurus = ASP.graph

  (* Task 3.1 *)
  fun make (S : (string * string seq) seq) : thesaurus =
    let
      fun unZip (word, synonym) = 
        Seq.map (fn syn => (word, syn)) synonym
    in
      ASP.makeGraph (Seq.flatten (Seq.map unZip S))
    end

  (* Task 3.2 *)
  fun numWords (T : thesaurus) : int =
    ASP.numVertices T

  fun synonyms (T : thesaurus) (w : string) : string seq =
    ASP.outNeighbors T w 

  (* Task 3.3 *)
  fun query (T : thesaurus) (w1 : string) (w2 : string) 
                                      : string seq seq =
    let 
      val asp = ASP.makeASP T w1
      val report = ASP.report asp 
    in
      report w2
    end

end
