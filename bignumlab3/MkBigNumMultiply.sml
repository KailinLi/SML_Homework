functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq


  infix 6 ++ --
  infix 7 **

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun x ** y =
    case (length x, length y) of
       (0, _) => empty()
     | (1, _) => if nth x 0 = ZERO then empty() else y
     | (lengthX, lengthY) => if (lengthX > lengthY) then y ** x else
      let
        val mid = lengthX div 2
        val (q, p) = (take (x, mid), drop (x, mid))
        val (s, r) = (take (y, mid), drop (y, mid))
        val pr = p ** r
        val qs = q ** s
        val directAdd = (p ++ q) ** (r ++ s)
        fun push (number, pow) = append (tabulate (fn _ => ZERO) pow, number)
      in
        (push (pr, mid * 2)) ++ (push ((directAdd -- pr -- qs), mid)) ++ qs
      end

    (*let
      fun push (array, (index, bit)) = if bit = ZERO then array ++ (singleton ZERO)
                                      else array ++ (append ((tabulate (fn x => ZERO) index), x))
    in
      iter push (singleton ZERO) (enum y)
    end*)

  val mul = op**
end
