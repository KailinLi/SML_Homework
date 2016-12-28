functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  (* Remove this line when you're done. *)

  infix 6 ++

  datatype carry = GEN | PROP | STOP

  fun x ++ y = 
   let

      fun rule array index = if index < (length array) then nth array index else ZERO
      val max = if (length x <= length y) then y else x
      val min = if (length x <= length y) then x else y

      fun directXOR max min = tabulate (fn index => case (rule max index, rule min index) of
                                              ((ZERO, ZERO) | (ONE, ONE)) => ZERO
                                            | ((ZERO, ONE) | (ONE, ZERO)) => ONE) (length max)

      fun directAND max min = tabulate (fn index => case (rule max index, rule min index) of
         (ZERO, ZERO) => STOP
       | ((ZERO, ONE) | (ONE, ZERO)) => PROP
       | (ONE, ONE) => GEN) (length max)
      fun copy (a, PROP) = a
        | copy (_, b)    = b
      val afterScan = scani copy STOP (directAND max min)
      val change = tabulate (fn index => if nth afterScan index = GEN then ONE else ZERO) (length afterScan)
      val afterAdd = append (singleton ZERO, change)
      val answer = directXOR afterAdd (directXOR max min)
    in
      answer
    end

  val add = op++
end
