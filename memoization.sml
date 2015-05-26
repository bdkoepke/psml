fun sum f 0 = 0
	| sum f n = (f n) + sum f (n - 1)
fun p 1 = 1
	| p n = sum (fn k => (p k) * (p (n - k))) (n - 1)

local 
	val limit = 100
	val memopad : int option Array.array = Array.array (limit, NONE)
in
	fun p' 1 = 1
		| p' n = sum (fn k => (p k) * p (n - k)) (n - 1)
	and p n =
		if n < limit then
			case Array.sub (memopad, n) of
					SOME r => r
				| NONE => 
					let
						val r = p' n
					in
						Array.update (memopad, n, SOME r);
						r
					end
		else
			p' n
end

val thunk =
	fn () => print "hello"
val _ = thunk ()

signature SUSP =
	sig
		type 'a susp
		val force : 'a susp -> 'a
		val delay : (unit -> 'a) -> 'a susp
	end

structure Susp :> SUSP =
	struct
		type 'a susp = unit -> 'a
		fun force t = t ()
		fun delay (t : 'a susp) =
			let
				exception Impossible
				val memo : 'a susp ref =
					ref (fn () => raise Impossible)
				fun t' () =
					let val r = t ()
					in memo := (fn () => r); r end
			in
				memo := t';
				fn () => (!memo)()
			end
	end

val t = Susp.delay (fn () => print "hello")
val _ = Susp.force t;
val _ = Susp.force t;

Control.lazysml := true;
open Lazy;

datatype lazy 'a stream = Cons of 'a * 'a stream
(*datatype 'a stream! = Cons of 'a * 'a stream
withtype 'a stream = 'a stream! Susp.susp *)

(*val Cons (h, t) = e
val Cons (h, t) = Susp.force e
fun stl (Cons (_, t)) = t
fun lstl (Cons (_, t)) = t
val rec lazy ones = Cons (1, ones)*)

(*val loopback : ('a susp -> 'a susp) -> 'a susp
fun ones_loop s = Susp.delay (fn () => Cons (1, s))
val ones = Susp.loopback ones_loop*)

fun loopback f =
	let
		exception Circular
		val r = ref (fn () => raise Circular)
		val t = fn () => (!r)()
	in
		r := f t ; t
	end
