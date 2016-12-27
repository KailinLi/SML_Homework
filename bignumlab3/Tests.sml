structure Tests =
struct
  val testsAdd : (IntInf.int * IntInf.int) list = [
    (2,2),
    (4000,3334),
    (9,1),
    (123, 937),
    (23413, 34324),
    (21,3243)
  ]

  val testsSub : (IntInf.int * IntInf.int) list = [
    (4000,3334),
    (3,2),
    (1024, 937),
    (2,1),
    (23423,543)
  ]

  val testsMul : (IntInf.int * IntInf.int) list = [
    (4000,3334),
    (3,2),
    (123, 937),
    (1,1000),
    (342,435345)
  ]

end
