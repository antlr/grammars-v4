defmacro defkv(kv) do
  Enum.map(kv, fn {k, v} ->
    quote do
      def unquote(k)(), do: unquote(v)
    end
  end)
end