fun hd (h::_) = h

exception Factorial
fun checked_factorial n = 
  if n < 0 then
    raise Factorial
  else if n = 0 then
    1
  else n * checked_factorial (n - 1)

local
  fun f 0 = 1
    | f n = n * f (n - 1)
in
  fun checked_factorial n =
    if n >= 0 then
      f n
    else
      raise Factorial
end

fun read_integer () =
  Option.mapPartial Int.fromString (TextIO.inputLine TextIO.stdIn)

fun factorial_driver () =
  let
    val input = read_integer ()
    fun result NONE = "Invalid input."
      | result (SOME a) =
        Int.toString (checked_factorial a)
    val _ = print (result input)
  in
    factorial_driver()
  end
  handle Factorial => print "Out of range."

fun factorial_driver () =
  let
    val input = read_integer ()
    val result =
      if isSome input then
        Int.toString (checked_factorial (valOf input))
      else
        "Invalid input."
    val _ = print result
  in
    factorial_driver()
  end
  handle EndOfFile => print "Done."

exception Change
fun change _ 0 = nil
  | change nil _ = raise Change
  | change (coin::coins) amount = 
    if coin > amount then
      change coins amount
    else
      (coin :: change (coin::coins) (amount - coin))
      handle Change => change coins amount;

change [5, 2] 16
