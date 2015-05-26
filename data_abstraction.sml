signature DICTIONARY =
sig
	type key = string
	type 'a entry = key * 'a
	type 'a dictionary
	exception Lookup of key
	val empty : 'a dictionary
	val insert : 'a dictionary * 'a entry -> 'a dictionary
	val lookup : 'a dictionary * key -> 'a
end

structure BinarySearchTree :> DICTIONARY =
struct
	type key = string
	type 'a entry = key * 'a
	datatype 'a tree =
		Empty |
		Node of 'a tree * 'a entry * 'a tree
	type 'a dictionary = 'a tree
	exception Lookup of key
	val empty = Empty
	fun insert (Empty, entry) =
		Node (Empty, entry, Empty)
	| insert (n as Node (l, e as (k, _), r), e' as (k', _)) =
		(case String.compare (k', k)
			of LESS => Node (insert (l, e'), e, r)
			 | GREATER => Node (l, e, insert (r, e'))
			 | EQUAL => n)
	fun lookup (Empty, k) = raise (Lookup k)
		| lookup (Node (l, (k, v), r), k') =
			(case String.compare (k', k)
				of EQUAL => v
				 | LESS => lookup (l, k')
				 | GREATER => lookup (r, k'))
end

structure RedBlackTree :> DICTIONARY =
struct
	type key = string
	type 'a entry = string * 'a
	datatype 'a dictionary = 
		Empty | 
		Red of 'a entry * 'a dictionary * 'a dictionary |
		Black of 'a entry * 'a dictionary * 'a dictionary
	val empty = Empty
	exception Lookup of key
	fun lookup (dictionary, key) =
		let
			fun lk (Empty) = raise (Lookup key)
				| lk (Red tree) = lk' tree
				| lk (Black tree) = lk' tree
			and lk' ((key1, datum1), left, right) =
				(case String.compare(key, key1)
				 of EQUAL => datum1
					| LESS => lk left
					| GREATER => lk right)
		in
			lk dictionary
		end
	fun restoreLeft
			(Black (z, Red (y, Red (x, d1, d2), d3), d4)) =
			Red (y, Black (x, d1, d2), Black (z, d3, d4))
		| restoreLeft
			(Black (z, Red (x, d1, Red (y, d2, d3)), d4)) =
			Red (y, Black (x, d1, d2), Black (z, d3, d4))
	fun restoreRight
			(Black (x, d1, Red (z, Red (y, d2, d3), d4))) =
			Red (y, Black (x, d1, d2), Black (z, d3, d4))
		| restoreRight dictionary = dictionary
	fun insert (dictionary, entry as (key, datum)) =
		let
			fun ins (Empty) = Red (entry, Empty, Empty)
				| ins (Red (entry1 as (key1, datum1), left, right)) =
					(case String.compare (key, key1)
						of EQUAL => Red (entry, left, right)
						 | LESS => Red (entry1, ins left, right)
						 | GREATER => Red (entry1, left, ins right))
				| ins (Black (entry1 as (key1, datum1), left, right)) =
					(case String.compare (key, key1)
						of EQUAL => Black (entry, left, right)
						 | LESS => restoreLeft (Black (entry1, ins left, right))
						 | GREATER => restoreRight (Black (entry1, left, ins right)))
		in
			case ins dictionary
				of Red (t as (_, Red _, _)) => Black t
				 | Red (t as (_, _, Red _)) => Black t
				 | dictionary => dictionary
		end
end
