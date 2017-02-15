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


  (* You must define the abstract kgramstats type *)
  type kgramstats = (string hist Table.table) * int

  fun makeStats (corpus : string) (maxK : int) : kgramstats =
    let
      val wordList = tokens (not o Char.isAlphaNum) corpus
      fun pick length = tabulate (fn index => (subseq wordList (index, length), nth wordList (index + length))) 
                        ((Seq.length wordList) - length)
      val getPiece = flatten (tabulate pick (maxK + 1))
      fun cmp (key1, key2) = collate String.compare (key1, key2)
      val get = collect cmp getPiece
      val makeHist = map (fn (key, sequence) => (key, histogram String.compare sequence)) get
    in
      (Table.fromSeq makeHist, maxK)
    end

  fun lookupExts (stats : kgramstats) (kgram : kgram) : (token * int) seq =
    case Table.find (#1 stats) kgram of
       NONE => empty()
     | SOME hist => hist

  fun maxK (stats : kgramstats) : int =
    #2 stats

end
