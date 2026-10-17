def sum(a, b) do
  a + b
end

def zero?(0) do
  true
end

def zero?(x) when is_integer(x) do
  false
end

def zero?(0), do: true

def zero?(x) when is_integer(x), do: false