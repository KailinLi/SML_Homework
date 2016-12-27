functor MkBruteForcePD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq

  fun parenDist (parens : paren seq) : int option =
  let
    fun count ((~1, _) | (0, CPAREN)) = ~1
      | count (n, CPAREN) = n - 1
      | count (n, OPAREN) = n + 1
    fun judgement inputSeq = if iter count 0 inputSeq = ~1 then false else true
    fun max (a, b) = if a > b then a else b
    fun loop (index: int) = 
      if nth parens index = OPAREN
        then
          let
            fun brute (i: int) = 
              if nth parens (i + index + 1) = CPAREN andalso i > 0 andalso judgement (subseq parens (index + 1, i))
                then i
              else 0
          in
            iter max 0 (tabulate brute ((length parens) - index - 1))
          end
        else 0
  in
    if judgement parens = false then NONE
    else
      SOME(iter max 0 (tabulate loop (length parens)))
  end

end