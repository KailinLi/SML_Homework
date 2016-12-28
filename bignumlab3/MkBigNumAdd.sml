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
      (*fun directAND max min = tabulate (fn index => case (rule max index, rule min index) of
                                              (ONE, ONE) => ONE
                                             | _ => ZERO) (length max)*)

      fun directAND max min = tabulate (fn index => case (rule max index, rule min index) of
         (ZERO, ZERO) => STOP
       | ((ZERO, ONE) | (ONE, ZERO)) => PROP
       | (ONE, ONE) => GEN) (length max)
      fun copy (a, PROP) = a
        | copy (_, b)    = b
      val shit = scani copy STOP (directAND max min)
      val fuck = tabulate (fn index => if nth shit index = GEN then ONE else ZERO) (length shit)
      val shit1 = append (singleton ZERO, fuck)
      val answer = directXOR shit1 (directXOR max min)


      (*fun judge ((false, _)|(_, ONE)) = false
        | judge (_, ZERO) = true
      fun check number = iter judge true number

      fun appendnumber number = case nth number (length number - 1) of
                               ZERO => append ((singleton ZERO), subseq number (0, length number - 1))
                             | _ => append ((singleton ZERO), number)

      fun caculate (ans, car) = 
        let
          val xornumber = directXOR ans car
          val andnumber = directAND ans car
        in
          if check andnumber then xornumber
          else caculate (appendnumber andnumber, xornumber)
        end
      val answer = caculate (max, min)*)
    in
      answer
    end

  val add = op++
end
