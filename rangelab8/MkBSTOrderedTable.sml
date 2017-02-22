functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)


  fun first (T : 'a table) : (key * 'a) option =
    case Tree.expose T of
       NONE => NONE
     | SOME {key, value, left, right} => 
      case Tree.expose left of
         NONE => SOME (key, value)
       | _ => first left

  fun last (T : 'a table) : (key * 'a) option = 
    case Tree.expose T of
       NONE => NONE
     | SOME {key, value, left, right} => 
      case Tree.expose right of
         NONE => SOME (key, value)
       | _ => last right

(*比较简单清晰，直接找到前一个元素*)
  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    last (#1 (Tree.splitAt (T, k)))

  fun next (T : 'a table) (k : key) : (key * 'a) option =
    first (#3 (Tree.splitAt (T, k)))

  fun join (L : 'a table, R : 'a table) : 'a table =
    Tree.join (L, R)

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
    Tree.splitAt (T, k)

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    (*#1 (Tree.splitAt (#3 (Tree.splitAt (T, low)), high)) 这样写，没有考虑开闭区间的问题*)
    let
      val firstSplit = case Tree.splitAt(T, low) of
       (_, SOME value, R) => join(singleton(low, value), R)
     | (_, NONE, R) => R 
      val secondSplit = case Tree.splitAt(firstSplit, high) of
         (L, SOME value, _) => join(L, singleton(high, value))
       | (L, NONE, _) => L
    in
      secondSplit
    end

end
