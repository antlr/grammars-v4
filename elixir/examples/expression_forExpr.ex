for n <- [1, 2, 3, 4], do: n * 2

for {type, name} when type != :guest <- users do
  String.upcase(name)
end

for <<r::8, g::8, b::8 <- pixels>>, do: {r, g, b}

for {language, parent} <- languages, grandparent = languages[parent], do: {language, grandparent}

for {language, parent} <- languages do
  grandparent = languages[parent]
  {language, grandparent}
end

for {language, parent} <- languages, grandparent <- [languages[parent]], do: {language, grandparent}

for <<c <- " hello world ">>, c != ?\s, into: "", do: <<c>>

for line <- IO.stream(), into: IO.stream() do
  String.upcase(line)
end

for x <- [1, 1, 2, 3], uniq: true, do: x * 2

for <<x <- "abcabc">>, uniq: true, into: "", do: <<x - 32>>

for <<x <- "AbCabCABc">>, x in ?a..?z, do: <<x>>