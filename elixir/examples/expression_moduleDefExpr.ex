defmodule Math do
  def sum(a, b) do
    a + b
  end
end

defmodule Math do
  def sum(a, b) do
    do_sum(a, b)
  end
  defp do_sum(a, b) do
    a + b
  end
end

defmodule Concat do
  def join(a, b, sep \\ " ") do
    a <> sep <> b
  end
end

defmodule Math do
  defmacro squared(x) do
    quote do
      x = unquote(x)
      x * x
    end
  end
end

defmodule Math do
  defmacro squared(x) do
    quote bind_quoted: [x: x] do
      x * x
    end
  end
end

defmodule Hygiene do
  defmacro no_interference do
    quote do
      a = 1
    end
  end
end

defmodule NoHygiene do
  defmacro interference do
    quote do
      var!(a) = 1
    end
  end
end

defmodule Hygiene do
  defmacro write do
    quote do
      a = 1
    end
  end
  defmacro read do
    quote do
      a
    end
  end
end

defmodule ContextHygiene do
  defmacro write do
    quote do
      var!(a, ContextHygiene) = 1
    end
  end
  defmacro read do
    quote do
      var!(a, ContextHygiene)
    end
  end
end

defmodule Hygiene do
  alias Map, as: M
  defmacro no_interference do
    quote do
      M.new()
    end
  end
end

defmodule Hygiene do
  alias Map, as: M
  defmacro no_interference do
    quote do
      M.new()
    end
  end
end

defmodule Hygiene do
  # This will expand to Elixir.Nested.hello()
  defmacro no_interference do
    quote do
      Nested.hello()
    end
  end
  # This will expand to Nested.hello() for
  # whatever is Nested in the caller
  defmacro interference do
    quote do
      alias!(Nested).hello()
    end
  end
end

defmodule Parent do
  defmodule Nested do
    def hello, do: "world"
  end
  require Hygiene
  Hygiene.no_interference()
  Hygiene.interference()
  #=> "world"
end

defmodule Mu do
  defmacro defkv(kv) do
    Enum.map(kv, fn {k, v} ->
      quote do
        def unquote(k)(), do: unquote(v)
      end
    end)
  end
end