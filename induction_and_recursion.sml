fun exp 0 = 1
  | exp n = 2 * exp (n - 1)

fun square (n:int) = n * n
fun double (n:int) = n + n
fun fast_exp 0 = 1
  | fast_exp n =
    if n mod 2 = 0 then
      square (fast_exp (n div 2))
    else
      double (fast_exp (n - 1))

fun skinny_fast_exp (0, a) = a
  | skinny_fast_exp (n, a) =
    if n mod 2 = 0 then
      skinny_fast_exp (n div 2,
        skinny_fast_exp (n div 2, a))
    else
      skinny_fast_exp (n - 1, 2 * a)

fun gen_skinny_fast_exp (b, 0, a) = a
  | gen_skinny_fast_exp (b, n, a) =
    if n mod 2 = 0 then
      gen_skinny_fast_exp (b * b, n div 2, a)
    else
      gen_skinny_fast_exp (b, n - 1, b * a)

local
  fun gen_skinny_fast_exp (b, 0, a) = a
    | gen_skinny_fast_exp (b, n, a) =
      if n mod 2 = 0 then
        gen_skinny_fast_exp (b * b, n div 2, a)
      else
        gen_skinny_fast_exp (b, n - 1, b * a)
in
  fun exp n = gen_skinny_fast_exp (2, n, 1)
end

fun gcd (m:int, 0):int = m
  | gcd (0, n:int):int = n
  | gcd (m:int, n:int):int =
    if m > n then
      gcd (m mod n, n)
    else
      gcd (m, n mod m)
