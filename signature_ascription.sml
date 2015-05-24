signature QUEUE =
  sig
    type 'a queue
    exception Empty
    val empty : 'a queue
    val insert : 'a * 'a queue -> 'a queue
    val remove : 'a queue -> 'a * 'a queue
  end

structure Queue :> QUEUE =
  struct
    type 'a queue = 'a list * 'a list
    val empty = (nil, nil)
    fun insert (x, (bs, fs)) = (x::bs, fs)
    exception Empty
    fun remove (nil, nil) = raise Empty
      | remove (bs, f::fs) = (f, (bs, fs))
      | remove (bs, nil) = remove (nil, rev bs)
  end

signature PRIORITY_QUEUE = 
  sig
    type t
    val lessThan : t * t -> bool
    type queue
    exception Empty
    val empty : queue
    val insert : t * queue -> queue
    val remove : queue -> t * queue
  end

signature STRING_PRIORITY_QUEUE = PRIORITY_QUEUE where type t = string

signature ORDERED =
  sig
    type t
    val lessThan : t * t -> bool
  end

structure IntLessThan : ORDERED =
  struct
    type t = int
    val lessThan = (op <)
  end

structure IntDiv : ORDERED =
  struct
    type t = int
    fun lessThan (m, n) = (n mod m = 0)
  end
