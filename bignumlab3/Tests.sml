structure Tests =
struct
  val testsAdd : (IntInf.int * IntInf.int) list = [
    (5,2),
    (4,3),
    (7,1),
    (123, 937),
    (23413, 34324),
    (21,3243)
  ]

  val testsSub : (IntInf.int * IntInf.int) list = [
    (3,2),
    (6,4),
    (1023, 1022),
    (23423,543)
  ]

  val testsMul : (IntInf.int * IntInf.int) list = [
    (3,4),
    (3,2),
    (123, 937),
    (1,1000),
    (342,435345)
  ]

end
