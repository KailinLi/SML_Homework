functor MkDivideAndConquerPD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq


  fun parenDist (parens : paren seq) : int option =
    let
      fun max (a, b) = if a > b then a else b
      fun max3 (a, b, c) = if max (a, b) > c then max (a, b) else c
      fun conquer ((leftOPA, leftCPA, leftMax), (rightOPA, rightCPA, rightMax)) =
        let
          fun match ([], [], max) = ([], [], max)
            | match (left, [], max) = (left, [], max)
            | match ([], right, max) = ([], right, max)
            | match (x::left, y::right, max) = match (left, right, y - x - 1)
          fun add ([], []) = []
            | add (left, []) = left
            | add (left, y::right) = y::add (left, right)
          fun append(list1, list2) = 
            case list1 of
               [] => list2
             | x::tl => x::append(tl, list2)
          fun reverse list =
            case list of
               [] => []
             | x::tl => append (reverse tl, x::[])
          fun prepare rightArray = if (leftOPA = []) then rightArray else (reverse rightArray)
          val afterMatch = match (leftOPA, (prepare rightCPA), 0) (*貌似复杂,实际每次的都可以conquer很多*)
        in
          (add ((#1 afterMatch), rightOPA), add (leftCPA, (#2 afterMatch)), max3 (leftMax, rightMax, (#3 afterMatch)))
        end
      fun divideAndConquer (array : (int * paren) seq)=
        case length array of
           1 => if ((#2 (nth array 0)) = OPAREN) then ((#1 (nth array 0))::[], [], 0)
                else ([], (#1 (nth array 0))::[], 0)
         | 2 => let
           val first = nth array 0
           val second = nth array 1
           val retrun = case (#2 first, #2 second) of
              (OPAREN, CPAREN) => ([], [], 0)
            | (OPAREN, OPAREN) => ((#1 second)::(#1 first)::[], [], 0)
            | (CPAREN, CPAREN) => ([], (#1 second)::(#1 first)::[], 0)
            | _ => ((#1 second)::[], (#1 first)::[], 0)
           in
             retrun
           end
         | _ => let
           val lengthArray = length array
           val divide = lengthArray div 2
         in
           conquer (divideAndConquer (take (array, divide)), divideAndConquer (drop (array, divide)))
         end
        val answer = divideAndConquer (enum parens)
    in
      if #1 answer = [] andalso #2 answer = [] then SOME (#3 answer)
      else NONE
    end


end
