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



  fun randomSentence (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    let
      val maxK = Stats.maxK stats
      val randomList = Rand.randomRealSeq seed NONE n
      (*使用rand的数组，产生循环*)
      fun addMore (sentance, newRandom) =
        let
        (*选取能取到的最长串*)
          val search = Stats.lookupExts stats (
              (drop (sentance, Int.max (0, (length sentance) - maxK) ))
            )
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
    (*map可以并行*)
      String.concatWith "\n" (toList 
        (map (fn new => getSentence new) (tabulate (fn i => i) n))
      )
    end

end
