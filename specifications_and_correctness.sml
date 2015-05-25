local
  exception PreCondition
  fun unchecked_fib 0 = 1
    | unchecked_fib 1 = 1
    | unchecked_fib n =
      unchecked_fib (n - 1) + unchecked_fib (n - 2)
in
  fun checked_fib n =
    if n < 0 then
      raise PreCondition
    else
      unchecked_fib n
end
