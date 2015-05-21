fn s => s ^ "\n";

(fn x => x)(0);
(fn x => x)(fn x => x)(0);

val I : 'a -> 'a = fn x => x
fun I (x:'a):'a = x
val I = fn x => x
fun I (x) = x

fun J x = I I x

val p = fn x:int => x+x;
val p = fn x:real => x+x;

(fn x => x + x)(3);

let
    val double = fn x => x + x
in
    (double 3, double 4)
end
