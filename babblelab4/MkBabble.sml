functor MkBabble(structure R : RANDOM210
                 structure KS : KGRAM_STATS
                 structure Util : SEQUENCE_UTIL
                 sharing KS.Seq = Util.Seq
                 sharing KS.Seq = R.Seq) : BABBLE =
struct
  structure Rand = R
  structure Stats = KS
  open Stats.Seq

  exception NoData

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  fun randomSentence (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    let
      val maxK = Stats.maxK stats
      val randomList = Rand.randomRealSeq seed NONE n 
      fun addMore (sentance, newRandom) = 
        let
          val search = Stats.lookupExts stats ((drop (sentance, Int.max (0, (length sentance) - maxK) )))
        in
          append(sentance, singleton(Util.choose search newRandom))
        end
      val getList = iter addMore (empty()) randomList
      val answer = String.concatWith " " (toList getList)
    in
      answer ^ "."
    end

  fun randomDocument (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    let
      val Seeds = R.randomIntSeq seed NONE n
      val wordLengths = R.randomIntSeq (R.fromInt(nth Seeds 0)) (SOME (5,11)) n
      fun getSentence (index) = 
        let
          val rSeed = (R.fromInt (nth Seeds index))
          val rLenght = nth wordLengths index
        in
          randomSentence stats rLenght rSeed
        end
    in
      String.concatWith " " (toList (map (fn new => getSentence new) (tabulate (fn i => i) n)))
    end

end
