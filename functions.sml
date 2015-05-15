val y : real = (fn x : real => Math.sqrt (Math.sqrt x)) (16.0)

val fourthroot : real -> real =
  fn x : real => Math.sqrt (Math.sqrt x)

fun fourthroot (x:real):real = Math.sqrt (Math.sqrt x);

fun srev (s:string):string = implode (rev (explode s))
fun pal (s:string):string = s ^ (srev s)
fun double (n:int):int = n + n
fun square (n:int):int = n * n
fun halve (n:int):int = n div 2
fun is_even (n:int):bool = (n mod 2 = 0)

val x:real = 2.0
fun f(x:real):real = x+x
fun g(y:real):real = x+y

fun h(x:real):real =
  let val x:real = 2.0 in x+x end * x

fun f(x:int):int = x*x
fun g(y:int):int = y*y

val x:real = 2.0
fun h(y:real):real = x+y

fun h'(x:real):real = x+x
