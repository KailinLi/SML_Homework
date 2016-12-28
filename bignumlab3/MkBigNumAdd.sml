functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  (* Remove this line when you're done. *)

  infix 6 ++

  datatype carry = GEN | PROP | STOP

  fun x ++ y = 
    (*let
      fun rule array index = if index < (length array) then nth array index else ZERO
      val max = if (length x <= length y) then y else x
      val min = if (length x <= length y) then x else y
      val directADD = tabulate (fn index => case (rule max index, rule min index) of
         (ZERO, ZERO) => STOP
       | ((ZERO, ONE) | (ONE, ZERO)) => PROP
       | (ONE, ONE) => GEN) (length max)
      val draft = append (directADD, singleton STOP)
      val turn = map (fn x => (1, x)) draft
      val shit = (singleton 0)
      fun change ((0, _), (_, STOP)) = (0, STOP)
        | change ((0, _), (_, GEN)) = (1, STOP)
        | change ((0, _), (_, PROP)) = (0, PROP)
        | change ((1, _), (_, STOP)) = (0, PROP)
        | change ((1, _), (_, GEN)) = (1, PROP)
        | change ((1, _), (_, PROP)) = (1, STOP)
      (*val answer = scani change (0, GEN) turn*)
      (*val final = if ((nth answer ((length answer) - 1)) = (0, STOP)) then take (answer, ((length answer) - 1)) else answer*)
    in
      map (fn (_, result) => if result = PROP then ONE else ZERO) answer
    end*)
  
    let

      fun rule array index = if index < (length array) then nth array index else ZERO
      val max = if (length x <= length y) then y else x
      val min = if (length x <= length y) then x else y

      fun directXOR max min = tabulate (fn index => case (rule max index, rule min index) of
                                              ((ZERO, ZERO) | (ONE, ONE)) => ZERO
                                            | ((ZERO, ONE) | (ONE, ZERO)) => ONE) (length max)
      fun directAND max min = tabulate (fn index => case (rule max index, rule min index) of
                                              (ONE, ONE) => ONE
                                             | _ => ZERO) (length max)

      fun judge ((false, _)|(_, ONE)) = false
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
      val answer = caculate (max, min)
    in
      answer
    end

  val add = op++
end
