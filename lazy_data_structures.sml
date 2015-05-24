Control.lazysml := true;
open Lazy;

datatype lazy 'a stream = Cons of 'a * 'a stream
val rec lazy ones = Cons (1, ones)

val Cons (h, t) = ones
val Cons (h, (Cons (h', t'))) = ones

fun shead (Cons (h, _)) = h
fun stail (Cons (_, s)) = s

fun lazy lazy_stail (Cons (_, s)) = s

val rec lazy s = (print "."; Cons (1, s))
val _ = stail s
val _ = stail s
val rec lazy s = (print "."; Cons (1, s))
val _ = lazy_stail s
val _ = stail s

fun smap f =
  let
    fun lazy loop (Cons (x, s)) =
      Cons (f x, loop s)
  in
    loop
  end

val map_increment = smap (fn n => n + 1)
val rec lazy natural_numbers = Cons (0, map_increment natural_numbers)

fun sfilter pred =
  let
    fun lazy loop (Cons (x, s)) =
      if pred x then
        Cons (x, loop s)
      else
        loop s
  in
    loop
  end

fun m mod n = m - n * (m div n)
fun divides m n = n mod m = 0
fun lazy sieve (Cons (x, s)) = 
  Cons (x, sieve (sfilter (not o (divides x)) s))

val natural_numbers_gt_2 = stail (stail natural_numbers)
val primes = sieve natural_numbers_gt_2

fun take 0 _ = nil
  | take n (Cons (x, s)) = x :: take (n - 1) s

val rec lazy s = Cons ((print "."; 1), s)
val Cons (h, _) = s;
val Cons (h, _) = s;
