fn s => s ^ "\n";

(fn x => x)(0);
(fn x => x)(fn x => x)(0);

val I : 'a -> 'a = fn x => x
fun I (x:'a):'a = x
val I = fn x => x
fun I (x) = x

val J = I I
fun J x = I I x

(* val l = nil @ nil *)

(* fn x:int => x + x
fn x:real => x + x *)

(fn x => x + x)(3);

let
    val double = fn x => x + x
in
    (double 3, double 4)
end

(*
fun #name {name=n:string, ...} = n
fn r : \{name:string,address:string,salary:int\} =>
   (\#name r, \#address r)

fn r => (\#name r, \#address r) *)
