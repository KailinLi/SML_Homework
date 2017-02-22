functor MkTableKGramStats(structure Util : SEQUENCE_UTIL
                          structure T : TABLE
                            where type Key.t = string Util.Seq.seq
                          sharing T.Seq = Util.Seq) : KGRAM_STATS =
struct
  structure Table = T
  structure Seq = T.Seq
  open Util
  open Seq

  type token = string
  type kgram = token seq


  type kgramstats = (string hist Table.table) * int

  fun makeStats (corpus : string) (maxK : int) : kgramstats =
    let
      val wordList = tokens (not o Char.isAlphaNum) corpus
      fun pick length = tabulate (fn index => 
                              (subseq wordList (index, length), nth wordList (index + length))) 
                        ((Seq.length wordList) - length)
      (*根据长度，选择单词片段，和之后的单词 拼成二元组*)
      val getPiece = flatten (tabulate pick (maxK + 1))
      fun cmp (key1, key2) = collate String.compare (key1, key2)
      (*collect W = O(nlogn) S = O(log^2n)需要cmp参数，逐字母比较*)
      val get = collect cmp getPiece
      val makeHist = map (fn (key, sequence) => (key, histogram String.compare sequence)) get
      (*把key对应的单词，制成词频表*)
    in
      (Table.fromSeq makeHist, maxK)
    end

  fun lookupExts (stats : kgramstats) (kgram : kgram) : (token * int) seq =
  (*Table.find W = O(logn)*)
    case Table.find (#1 stats) kgram of
       NONE => empty()
     | SOME hist => hist

  fun maxK (stats : kgramstats) : int =
    #2 stats

end
