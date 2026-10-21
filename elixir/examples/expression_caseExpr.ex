case {1, 2, 3} do
  {4, 5, 6} -> "This clause won't match"
  {1, x, 3} -> "This clause will match and bind x to 2 in this clause"
  _         -> "This clause would match any value"
end

case 10 do
  ^x ->
    "Won't match"
  _ ->
    "Will match"
end
