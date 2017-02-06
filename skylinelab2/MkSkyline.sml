functor MkSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Primitives
  open Seq

  datatype positionType = Left|Right;
  fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
   let
      fun compare ((left, _), (right, _)) =
        if left < right then LESS else
          if left = right then EQUAL else GREATER

      fun transfer (l,h,r) =
        let
          val a = singleton (l, h)
          val b = singleton (r, 0)
        in
          merge compare a b
        end;     
    val newSkyline = map transfer buildings

    fun myMerge (left: (int*int)seq, right: (int*int)seq): (int*int) seq=
      let
        val tagLeft = map (fn (l, h) => (l, h, Left)) left
        val tagRight = map (fn (l, h) => (l, h, Right)) right
        val init :(int*int*positionType) seq = merge (fn((l,_,_),(r,_,_)) => if l < r then LESS else if l = r then EQUAL else GREATER) tagLeft tagRight

        val filterL:int seq = map (fn(l,h,pos)=>if(pos=Right) then h else ~1) init
        val filterR:int seq = map (fn(l,h,pos)=>if(pos=Left) then h else ~1) init
        fun copy (x,y)=if y> ~1 then y else x
        val getleft = scani copy ~1 filterL
        val getRight = scani copy ~1 filterR

        fun modifyL(x: int,(l, h, pos):(int*int*positionType))=
          if pos = Left then
            if x > h then (l, x, pos)
              else (l, h, pos)
          else
            (l, h, pos)
        fun modifyR(x,(l, h, pos))=
          if pos = Right then
            if x > h then (l, x, pos)
              else (l, h, pos)
          else
            (l, h, pos)
        val changeL:(int*int*positionType) seq = map2 modifyL getleft init
        val changeR = map2 modifyR getRight init
        val change = map2 (fn((l1, l2, pos1),(r1, r2, pos2))=>if pos1=Left then (l1, l2, pos1) else (r1, r2, pos2)) changeL changeR

      in
        map (fn (l, r, pos) => (l, r)) change
      end;

      val result = reduce myMerge (singleton (~1, ~1))  newSkyline
      val finalResult = drop (result, 1)

      fun judge (0, _) = true
        | judge (index, element:(int * int)) = if (#2 element) = (#2 (nth finalResult (index - 1)))
                                      then false else true
      val answer = filterIdx judge finalResult
   in
     answer
  end


  (* fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
  let
    val leftArray = map (fn (l, h, r) => (l, 0)) buildings
    val rightArray = map (fn (l, h, r) => (r, 0)) buildings

    val search = fn ((x, high), (l, h, r)) => if (l <= x) andalso (x < r) andalso (high < h) then (x, h)
                                                                            else (x, high)
    fun findMax (x, high) : (int * int) = iter search (x, high) buildings
    val result = merge (fn ((i, h1), (j, h2)) => if i < j then LESS else GREATER) (map findMax leftArray) (map findMax rightArray)(*merge W = O(n) S = O(log n)*)
    val finalResult = sort (fn ((i, h1), (j, h2)) => if i < j then LESS else GREATER) result (*sort W = O(nlogn) S = O (logn)*)
    
    fun judge (0, _) = true
        | judge (index, element:(int * int)) = if (#2 element) = (#2 (nth finalResult (index - 1)))
                                      then false else true
    val answer = filterIdx judge finalResult (*W = O(n)*)
  in
    answer
  end *)

end
