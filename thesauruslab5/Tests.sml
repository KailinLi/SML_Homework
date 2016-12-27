structure Tests =
struct

  structure T = MkTreapTable(structure HashKey = IntElt)
  structure S = T.Seq
  open S

  type edge = int * int

  (*a trivial test that has a graph containing 2 vertices and an edge*)
  val edgeseq = [(1,2)]
  val edgeseq2 = [(1,2),(2,3),(3,4),(2,4),(1,5),(5,4),(5,6),(6,7)]
  val testfile = "input/thesaurus.txt"
  val testfile2 = "input/simpletest.txt"

  (* The following are required *)
  val testsNum = [edgeseq, edgeseq2];

  val testsOutNeighbors = [(edgeseq, 1), (edgeseq, 2)]

  val testsReport = [((edgeseq, 1), 2), ((edgeseq2, 1), 4), ((edgeseq2, 1), 7)]

  val testsNumWords =  [testfile, testfile2]

  val testsSynonyms =
    [(testfile2, "HANDSOME"),
     (testfile2, "VINCENT"),
     (testfile2, "PRETTY"),
     (testfile, "GOOD")]

  val testsQuery =
    [(testfile2, ("HANDSOME", "YOLO")), (testfile, ("GOOD", "BAD"))]

end
