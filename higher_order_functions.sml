val x = 2
val y = x * x
val x = y * x
val y = x * y

val x = 2
fun f y = x + y
val x = 3
val z = f 4

fun map' (f, nil) = nil
  | map' (f, h::t) = (f h) :: map' (f, t)

val s = map' (fn x => x + 1, [1, 2, 3, 4])

val constantly = fn k => (fn a => k)

fun constantly k a = k

fun map f nil = nil
  | map f (h::t) = (f h) :: (map f t)

fun curry f x y = f (x, y)

fun map f l = ((curry map') f) l

fun add_up nil = 0
  | add_up (h::t) = h + add_up t

fun mul_up nil = 1
  | mul_up (h::t) = h * mul_up t

fun foldr (f, z, l) =
  let
    fun foldr nil = z
      | foldr (h::t) = f (h, foldr t)
  in
    foldr l
  end

fun reduce (f, z, l) = foldr (f, z, l)

fun add_up l = reduce (op +, 0, l)
fun mul_up l = reduce (op *, 0, l)

fun mystery l = reduce (op ::, nil, l)

fun curry_foldr (f, z) =
  let
    fun curry_foldr nil = z
      | curry_foldr (h::t) = f (h, curry_foldr t)
  in
    curry_foldr
  end

fun curried_foldr (f, z) nil = z
  | curried_foldr (f, z) (h::t) =
    f (z, curried_foldr (f, z) t)

fun append (nil, l) = l
  | append (h::t, l) = h :: append (t, l)

fun curried_append nil l = l
  | curried_append (h::t) l = h :: curried_append t l

fun staged_append nil = (fn l => l)
  | staged_append (h::t) =
    let
      val tail_appender = staged_append t
    in
      fn l => h :: tail_appender l
    end
