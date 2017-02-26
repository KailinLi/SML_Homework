functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare


  (* Define this yourself *)
  type countTable = int table table

  fun makeCountTable (S : point seq) : countTable =
    let
      val sortX = Seq.sort (fn (x1, x2) => compareKey(#1 x1, #1 x2)) S
      val makeKey = Seq.map (fn point => (#2 point, 0)) S
      val insertion = fn (first, next) => OrdTable.insert (fn (_, a) => a) next first
      (*仅仅用来处理重复元素，因为x y都是唯一的*)
      val getTable = Seq.iterh (insertion) (empty()) makeKey
      (*保存每一步的结果，正好可以符合对于每个x单独处理的要求*)
      val resultTable = if (Seq.length S = 0) then Seq.empty() else 
        Seq.append (Seq.drop (#1 getTable, 1), Seq.singleton(#2 getTable))
        (*第一个元素是空，丢弃*)
      val mix = Seq.zip (Seq.map #1 sortX) resultTable
    in
      OrdTable.fromSeq mix
    end

  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
    let
      val selectX = getRange T (xLeft, xRght)
      val right = if (size selectX = 0) then 0 else size (getRange (#2 (valOf(last selectX))) (yLo, yHi))
      val left = if (size selectX = 0) then 0 else size (getRange (#2 (valOf(first selectX))) (yLo, yHi))
    in
      if (size selectX = 0) then 0 else right - left + 1
    end
end
