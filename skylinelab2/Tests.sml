structure Tests =
struct

  val tests = List.map ArraySequence.% [
    [(1,1,2)],
    [(1,1,3),(2,2,4)],
    [(1,5,8),(2,3,6),(3,1,7)],
    [(4,5,20),(1,3,5),(2,4,6),(8,7,11),(12,11,13),(10,10,14),(17,2,21)],
    [(20,10000,90000)]
  ]

end