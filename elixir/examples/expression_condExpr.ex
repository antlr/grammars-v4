cond do
  2 + 2 == 5 ->
    "This will not be true"
  2 * 2 == 3 ->
    "Nor this"
  1 + 1 == 2 ->
  "But this will"
end

cond do
  hd([1, 2, 3]) -> "1 is considered as true"
end