functor MkSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Primitives
  open Seq


  fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
  let
    val leftArray = map (fn (l, h, r) => (l, 0)) buildings
    val rightArray = map (fn (l, h, r) => (r, 0)) buildings

    val search = fn ((x, high), (l, h, r)) => if (l <= x) andalso (x < r) andalso (high < h) then (x, h)
                                                                            else (x, high)
    fun findMax (x, high) : (int * int) = iter search (x, high) buildings
    val result = merge (fn ((i, h1), (j, h2)) => if i < j then LESS else GREATER) (map findMax leftArray) (map findMax rightArray)
    val finalResult = sort (fn ((i, h1), (j, h2)) => if i < j then LESS else GREATER) result
    
    fun judge (0, _) = true
        | judge (index, element:(int * int)) = if (#2 element) = (#2 (nth finalResult (index - 1)))
                                      then false else true
    val answer = filterIdx judge finalResult
  in
    answer
  end

end
