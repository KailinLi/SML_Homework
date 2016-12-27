functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  infix 6 ++ --

  fun x ++ y = BNA.add (x, y)
  fun x -- y = 
    let
      fun rule array index = if index < (length array) then nth array index else ZERO
      fun reverse number = tabulate (fn index => if rule number index = ZERO then ONE else ZERO) (length x)
      val new = (reverse y) ++ singleton ONE
      val minus = take (x ++ new, length x)
      fun lastStep number :bit seq =
        let
          val last = nth number (length number - 1)
        in
          if last = ZERO then lastStep (subseq number (0, length number - 1))
          else number
        end
    in
      lastStep minus
    end
  val sub = op--
end
