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
    let
      fun push (array, (index, bit)) = if bit = ZERO then array ++ (singleton ZERO)
                                      else array ++ (append ((tabulate (fn x => ZERO) index), x))
    in
      iter push (singleton ZERO) (enum y)
    end

  val mul = op**
end
