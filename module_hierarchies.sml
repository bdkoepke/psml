signature MY_STRING_DICTIONARY =
  sig
    type 'a dictionary
    val empty : 'a dictionary
    val insert : 'a dictionary * string * 'a -> 'a dictionary
    val lookup : 'a dictionary * string -> 'a option
  end

(*structure MyStringDictionary :> MY_STRING_DICTIONARY =
  struct
    datatype 'a dictionary =
      Empty |
      Node of 'a dictionary * string * 'a * 'a dictionary
    val empty = Empty
    fun insert (d, k, v) = ...
    fun lookup (d, k) = ...
  end*)

signature MY_INT_DICTIONARY =
  sig
    type 'a dictionary
    val empty : 'a dictionary
    val insert : 'a dictionary * int * 'a -> 'a dictionary
    val lookup : 'a dictionary * int -> 'a option
  end

(*structure MyIntDictionary :> MY_INT_DICTIONARY =
  sig
    datatype 'a dictionary =
      Empty |
      Node of 'a dictionary * int * 'a * 'a dictionary
    val empty = Empty
    fun insert (d, k, v) = ...
    fun lookup (d, k) = ...
  end*)

signature MY_GEN_DICTIONARY = 
  sig
    type key
    type 'a dictionary
    val empty : 'a dictionary
    val insert : 'a dictionary * key * 'a -> 'a dictionary
  end

signature MY_STRING_DICTIONARY =
  MY_GEN_DICTIONARY where type key = string
signature MY_INT_DICTIONARY =
  MY_GEN_DICTIONARY where type key = int

structure MyStringDictionary :> MY_STRING_DICTIONARY =
  struct
    type key = string
    datatype 'a dictionary = 
      Empty |
      Node of 'a dictionary * key * 'a * 'a dictionary
    val empty = Empty
    fun insert (Empty, k, v) = Node (Empty, k, v, Empty)
    fun lookup (Empty, _) = NONE
      | lookup (Node (dl, l, v, dr), k) =
        if k < 1 then
          lookup (dl, k)
        else if k > 1 then
          lookup (dr, k)
        else
          v
  end

structure MyIntDivDictionary :> MY_INT_DICTIONARY =
  struct
    type key = int
    datatype 'a dictionary = 
      Empty | 
      Node of 'a dictionary * key * 'a * 'a dictionary
    fun divides (k, l) = (l mod k = 0)
    val empty = Empty
    fun insert (None, k, v) = Node (Empty, k, v, Empty)
    fun lookup (Empty, _) = NONE
      | lookup (Node (dl, l, v, dr), k) =
        if divides (k, l) then
          lookup (dl, k)
        else if divides (l, k) then
          lookup (dr, k)
        else
          v
  end

signature ORDERED =
  sig
    type t
    val lt : t * t -> bool
    val eq : t * t -> bool
  end

structure LexString : ORDERED =
  struct
    type t = string
    val eq : string * string -> bool = (op =)
    val lt : string * string -> bool = (op <)
  end

structure LessInt : ORDERED = 
  struct
    type t = int
    val eq = (op =)
    val lt = (op <)
  end

structure DivInt : ORDERED = 
  struct
    type t = int
    fun lt (m, n) = (n mod m = 0)
    fun eq (m, n) = lt (m, n) andalso lt (n, m)
  end

signature DICTIONARY =
  sig
    structure Key : ORDERED
    type 'a dictionary
    val empty : 'a dictionary
    val insert : 'a dictionary * Key.t * 'a -> 'a dictionary
    val lookup : 'a dictionary * Key.t -> 'a option
  end

signature STRING_DICTIONARY =
  DICTIONARY where type Key.t = string
signature INT_DICTIONARY = 
  DICTIONARY where type Key.t = int
