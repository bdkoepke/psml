signature QUEUE =
  sig
    type 'a queue
    exception Empty
    val empty : 'a queue
    val insert : 'a * 'a queue -> 'a queue
    val remove : 'a queue -> 'a * 'a queue
  end

signature QUEUE_WITH_EMPTY =
  sig
    include QUEUE
    val is_empty : 'a queue -> bool
  end

signature QUEUE_AS_LISTS =
  QUEUE where type 'a queue = 'a list * 'a list

(* signature QUEUE_AS_LISTS_AS_LIST =
  QUEUE_AS_LISTS where type 'a queue = 'a list *)

signature QUEUE_AS_LIST =
  QUEUE where type 'a queue = 'a list

structure Queue =
  struct
    type 'a queue = 'a list * 'a list
    exception Empty
    val empty = (nil, nil)
    fun insert (x, (b,f)) = (x::b, f)
    fun remove (nil, nil) = raise Empty
      | remove (bs, nil) = remove (nil, rev bs)
      | remove (bs, f::fs) = (f, (bs, fs))
  end

val q = Queue.insert (1, ([6, 5, 4], [1, 2, 3]))

val q = Queue.insert (2, Queue.insert (1, Queue.empty))

structure Q = Queue
val q = Q.insert (2, Q.insert (1, Q.empty))

open Queue
val q = insert (2, insert (1, empty))
