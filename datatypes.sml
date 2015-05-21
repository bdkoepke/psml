datatype suit = Spades | Hearts | Diamonds | Clubs

fun outranks (Spades, Spades) = false
  | outranks (Spades, _) = true
  | outranks (Hearts, Spades) = false
  | outranks (Hearts, Hearts) = false
  | outranks (Hearts, _) = true
  | outranks (Diamonds, Clubs) = true
  | outranks (Diamonds, _) = false
  | outranks (Clubs, _) = false

datatype 'a option = NONE | SOME of 'a

fun expt (NONE, n) = expt (SOME 2, n)
  | expt (SOME b, 0) = 1
  | expt (SOME b, n) =
    if n mod 2 = 0 then
      expt (SOME (b * b), n div 2)
    else
      b * expt (SOME b, n - 1)

type entry = { name:string, spouse:string option }

fun reciprocal 0 = NONE
  | reciprocal n = SOME (1 div n)

datatype 'a tree = 
  Empty |
  Node of 'a tree * 'a * 'a tree

fun max (a, b) =
  if a > b then
    a
  else
    b

fun height Empty = 0
  | height (Node (lft, _, rht)) = 
    1 + max (height lft, height rht)

fun size Empty = 0
  | size (Node (lft, _, rht)) = 
    1 + size lft + size rht

datatype 'a tree = 
  Empty |
  Node of 'a * 'a tree list

datatype 'a tree = 
  Empty |
  Node of 'a * 'a forest
and 'a forest =
  None | 
  Tree of 'a tree * 'a forest

fun size_tree Empty = 0
  | size_tree (Node (_, f)) = 1 + size_forest f
and size_forest None = 0
  | size_forest (Tree (t, f')) = size_tree t + size_forest f'

datatype 'a tree = 
  Empty | 
  Node of 'a branch * 'a branch
and 'a branch =
  Branch of 'a * 'a tree

fun collect Empty = nil
  | collect (Node (Branch (ld, lt), Branch (rd, rt))) =
    ld :: rd :: (collect lt) @ (collect rt)

datatype int_or_string = 
  Int of int |
  String of string

type int_or_string_tree = 
  int_or_string tree

datatype expr =
  Numeral of int |
  Plus of expr * expr |
  Times of expr * expr

fun eval (Numeral n) = Numeral n
  | eval (Plus (e1, e2)) =
    let
      val Numeral n1 = eval e1
      val Numeral n2 = eval e2
    in
      Numeral (n1+n2)
    end
  | eval (Times (e1, e2)) =
    let
      val Numeral n1 = eval e1
      val Numeral n2 = eval e2
    in
      Numeral (n1*n2)
    end

datatype expr =
  Numeral of int |
  Plus of expr * expr |
  Times of expr * expr |
  Recip of expr

fun eval (Numeral n) = Numeral n
  | eval (Plus (e1, e2)) =
    let
      val Numeral n1 = eval e1
      val Numeral n2 = eval e2
    in
      Numeral (n1+n2)
    end
  | eval (Times (e1, e2)) =
    let
      val Numeral n1 = eval e1
      val Numeral n2 = eval e2
    in
      Numeral (n1*n2)
    end
  | eval (Recip e) =
    let
      val Numeral n = eval e
    in
      Numeral (1 div n)
    end
