string = "hello"

is_binary(string)

?a

?l

"\u0061" == "a"

0x0061 = 97 = ?a

IO.inspect("hello", binaries: :as_binaries)

String.split("1  2  3", " ", [trim: true])

[{:trim, true}] == [trim: true]

list[:a]

(&is_function/1).(fun)

&Math.zero?/1

users = update_in users[:mary].languages, fn languages -> List.delete(languages, "Clojure") end

DefaultTest.dowork 123

defmodule Concat do
  # A function head declaring defaults
  def join(a, b \\ nil, sep \\ " ")
  def join(a, b, _sep) when is_nil(b) do
    a
  end
  def join(a, b, sep) do
    a <> sep <> b
  end
end

IO.puts Concat.join("Hello", "world")      #=> Hello world

IO.puts Concat.join("Hello", "world", "_") #=> Hello_world

IO.puts Concat.join("Hello")               #=> Hello

defmodule Concat do
  def join(a, b) do
    IO.puts "***First join"
    a <> b
  end
  def join(a, b, sep \\ " ") do
    IO.puts "***Second join"
    a <> sep <> b
  end
end

Concat.join "Hello", "world", "_"

defmodule Recursion do
  def print_multiple_times(msg, n) when n > 0 do
    IO.puts(msg)
    print_multiple_times(msg, n - 1)
  end
  def print_multiple_times(_msg, 0) do
    :ok
  end
end

defmodule Math do
  def sum_list([head | tail], accumulator) do
    sum_list(tail, head + accumulator)
  end
  def sum_list([], accumulator) do
    accumulator
  end
end

IO.puts Math.sum_list([1, 2, 3], 0) #=> 6

defmodule Math do
  def double_each([head | tail]) do
    [head * 2 | double_each(tail)]
  end
  def double_each([]) do
    []
  end
end

Enum.reduce([1, 2, 3], 0, fn(x, acc) -> x + acc end)

Enum.reduce([1, 2, 3], 0, &+/2)

Enum.map([1, 2, 3], &(&1 * 2))

&Kernel.+/2

add = &(&1 + &2)

'abcdaabccc' |> Enum.sort() |> Enum.chunk_by(&Function.identity/1)

import List

import List, only: :functions

import List, only: [flatten: 1, keyfind: 4]

import File.Stream, only: [__build__: 3]

quote do
  unquote("hello")
end

quote unquote: false do
  unquote("hello")
end

Enum.map([1, 2, 3], fn x -> x * 2 end)

Enum.map(%{1 => 2, 3 => 4}, fn {k, v} -> k * v end)

Enum.map(1..3, fn x -> x * 2 end)

Enum.reduce(1..3, 0, &+/2)

odd? = &(rem(&1, 2) != 0)

Enum.filter(1..3, odd?)

1..100_000 |> Enum.map(&(&1 * 3)) |> Enum.filter(odd?) |> Enum.sum()

Enum.sum(Enum.filter(Enum.map(1..100_000, &(&1 * 3)), odd?))

1..100_000 |> Stream.map(&(&1 * 3)) |> Stream.filter(odd?) |> Enum.sum

1..100_000 |> Stream.map(&(&1 * 3)) |> Stream.filter(odd?)

stream = Stream.cycle([1, 2, 3])

Enum.take(stream, 10)

Stream.unfold("hello", &String.next_codepoint/1)

stream = File.stream!("path/to/file")
%File.Stream{
  line_or_bytes: :line,
  modes: [:raw, :read_ahead, :binary],
  path: "path/to/file",
  raw: true
}

spawn(fn -> 1 + 2 end)

Process.alive?(self())

send(self(), {:hello, "world"})

receive do
  {:hello, msg} -> msg
  {:world, _msg} -> "won't match"
end

receive do
  {:hello, msg}  -> msg
after
  1_000 -> "nothing after 1s"
end

spawn(fn -> send(parent, {:hello, self()}) end)

receive do
  {:hello, pid} -> "Got hello from #{inspect pid}"
end

spawn_link(fn -> raise "oops" end)

Task.start(fn -> raise "oops" end)

defmodule KV do
  def start_link do
    Task.start_link(fn -> loop(%{}) end)
  end
  defp loop(map) do
    receive do
      {:get, key, caller} ->
        send caller, Map.get(map, key)
        loop(map)
      {:put, key, value} ->
        loop(Map.put(map, key, value))
    end
  end
end

Process.register(pid, :kv)
send(:kv, {:get, :hello, self()})

case 1 do
  x when hd(x) -> "Won't match"
  x -> "Got #{x}"
end

f = fn
  x, y when x > 0 -> x + y
  x, y -> x * y
end

cond do
  2 + 2 == 5 ->
    "This will not be true"
  2 * 2 == 3 ->
    "Nor this"
  1 + 1 == 2 ->
    "But this will"
end

cond do
  hd([1, 2, 3]) ->
    "1 is considered as true"
end

if nil do
  "This won't be seen"
else
  "This will"
end

query = from w in Weather, where: w.prcp > 0, where: w.temp < 20, select: w

query = from w in Weather,
  where: w.prcp > 0,
  where: w.temp < 20,
  select: w

if true, do: "This will be seen", else: "This won't"

map = %{:a => 1, 2 => :b}

%{:a => a} = %{:a => 1, 2 => :b}

Map.to_list(%{:a => 1, 2 => :b})

users = [
  john: %{name: "John", age: 27, languages: ["Erlang", "Ruby", "Elixir"]},
  mary: %{name: "Mary", age: 29, languages: ["Elixir", "F#", "Clojure"]}
]

users[:john].age

users = put_in users[:john].age, 31

users = update_in users[:mary].languages, fn languages -> List.delete(languages, "Clojure") end

File.read!("path/to/file/unknown")

{:ok, body} = File.read("path/to/file/unknown")

[?a, ?b, ?c]

alias Foo.Bar, as: Bar

require Foo

import Foo

use Foo

defmodule Stats do
  alias Math.List, as: List
  # In the remaining module definition List expands to Math.List.
end

defmodule AssertionTest do
  use ExUnit.Case, async: true
  test "always pass" do
    assert true
  end
end

defmodule Example do
  require Feature
  Feature.__using__(option: :value)
end

:"Elixir.List".flatten([1, [2], 3])

defmodule Foo do
  defmodule Bar do
    defmodule Baz do
    end
  end
end

alias MyApp.{Foo, Bar, Baz}

defmodule MyServer do
  @moduledoc "My server code."
end

defmodule Math do
  @moduledoc """
    Provides math-related functions.
    ## Examples
    iex> Math.sum(1, 2)
    3
  """
  @doc """
    Calculates the sum of two numbers.
  """
  def sum(a, b), do: a + b
end

defmodule MyServer do
  @initial_state %{host: "127.0.0.1", port: 3456}
  IO.inspect @initial_state
end

defmodule MyServer do
  @my_data 14
  def first_data, do: @my_data
  @my_data 13
  def second_data, do: @my_data
end

defmodule MyApp.Status do
  @service URI.parse("https://example.com")
  def status(email) do
    SomeHttpClient.get(@service)
  end
end

defmodule MyApp.Status do
  def status(email) do
    SomeHttpClient.get(%URI{
      authority: "example.com",
      host: "example.com",
      port: 443,
      scheme: "https"
    })
  end
end

def some_function, do: do_something_with(@example)

def another_function, do: do_something_else_with(@example)

defmodule Foo do
  Module.register_attribute __MODULE__, :param, accumulate: true
  @param :foo
  @param :bar
  # here @param == [:bar, :foo]
end

defmodule MyTest do
  use ExUnit.Case, async: true
  @tag :external
  @tag os: :unix
  test "contacts external service" do
    # ...
  end
end

defmodule User do
  defstruct name: "John", age: 27
end

%User{age: 27, name: "John"}

%{jane | oops: :field}

%User{} = %{}

john.__struct__

Enum.each(john, fn {field, value} -> IO.puts(value) end)

jane = Map.put(%User{}, :name, "Jane")

defmodule Product do
  defstruct [:name]
end

defmodule Car do
  @enforce_keys [:make]
  defstruct [:model, :make]
end

defmodule Utility do
  def type(value) when is_binary(value), do: "string"
  def type(value) when is_integer(value), do: "integer"
  # ... other implementations ...
end

defprotocol Utility do
  @spec type(t) :: String.t()
  def type(value)
end

defimpl Utility, for: BitString do
  def type(_value), do: "string"
end

defimpl Utility, for: Integer do
  def type(_value), do: "integer"
end

defimpl Size, for: BitString do
  def size(string), do: byte_size(string)
end

defimpl Size, for: Map do
  def size(map), do: map_size(map)
end

defimpl Size, for: Tuple do
  def size(tuple), do: tuple_size(tuple)
end

defprotocol Size do
  @fallback_to_any true
  def size(data)
end

defimpl Size, for: Any do
  def size(_), do: 0
end

inspect &(&1+2)

for n <- [1, 2, 3, 4], do: n * n

for n <- 1..4, do: n * n

dirs = ["/home/mikey", "/home/james"]
for dir <- dirs,
    file <- File.ls!(dir),
    path = Path.join(dir, file),
    File.regular?(path) do
  File.stat!(path).size
end

regex = ~r/foo|bar/

~s(this is a string with "double" quotes, not 'single' ones)

~w(foo bar bat)a

try do
  something()
rescue
  e ->
    Logger.error(Exception.format(:error, e, __STACKTRACE__))
    reraise e, __STACKTRACE__
end

try do
  Enum.each(-50..50, fn x ->
    if rem(x, 13) == 0, do: throw(x)
  end)
  "Got nothing"
catch
  x -> "Got #{x}"
end

Enum.find(-50..50, &(rem(&1, 13) == 0))

spawn_link(fn -> exit(1) end)

try do
  exit("I am exiting")
catch
  :exit, _ -> "not really"
end

try do
  IO.write(file, "ola")
  raise "oops, something went wrong"
after
  File.close(file)
end

try do
  1 / x
rescue
  ArithmeticError ->
    :infinity
else
  y when y < 1 and y > -1 ->
    :small
  _ ->
    :large
end

try do
  raise "fail"
  what_happened = :did_not_raise
rescue
  _ -> what_happened = :rescued
end

if true do
  :this
else
  :that
end

if variable? do
  Call.this()
else
  Call.that()
end

if variable?, do: Call.this(), else: Call.that()

defmodule Math do
  def add(a, b) do
    a + b
  end
end

defmodule(Math, [
  {:do, def(add(a, b), [{:do, a + b}])}
])

Base.encode16(:crypto.hash(:sha256, "Elixir"))

def application do
  [extra_applications: [:crypto]]
end

digraph = :digraph.new()
coords = [{0.0, 0.0}, {1.0, 0.0}, {1.0, 1.0}]
[v0, v1, v2] = (for c <- coords, do: :digraph.add_vertex(digraph, c))
:digraph.add_edge(digraph, v0, v1)
:digraph.add_edge(digraph, v1, v2)
:digraph.get_short_path(digraph, v0, v2)

song = "
Mary had a little lamb,
His fleece was white as snow,
And everywhere that Mary went,
The lamb was sure to go."

round(number()) :: integer()

defmodule Person do
   @typedoc """
   A 4 digit year, e.g. 1984
   """
   @type year :: integer
   @spec current_age(year) :: integer
   def current_age(year_of_birth), do: "implementation"
end

@type error_map :: %{
   message: String.t,
   line_number: integer
}

defmodule LousyCalculator do
  @spec add(number, number) :: {number, String.t}
  def add(x, y), do: {x + y, "You need a calculator to do that?!"}
  @spec multiply(number, number) :: {number, String.t}
  def multiply(x, y), do: {x * y, "Jeez, come on!"}
end

defmodule LousyCalculator do
  @typedoc """
  Just a number followed by a string.
  """
  @type number_with_remark :: {number, String.t}
  @spec add(number, number) :: number_with_remark
  def add(x, y), do: {x + y, "You need a calculator to do that?"}
  @spec multiply(number, number) :: number_with_remark
  def multiply(x, y), do: {x * y, "It is like addition on steroids."}
end

defmodule QuietCalculator do
  @spec add(number, number) :: number
  def add(x, y), do: make_quiet(LousyCalculator.add(x, y))
  @spec make_quiet(LousyCalculator.number_with_remark) :: number
  defp make_quiet({num, _remark}), do: num
end

defmodule Parser do
  @doc """
  Parses a string.
  """
  @callback parse(String.t) :: {:ok, term} | {:error, atom}
  @doc """
  Lists all supported file extensions.
  """
  @callback extensions() :: [String.t]
end

defmodule JSONParser do
  @behaviour Parser
  @impl Parser
  def parse(str), do: {:ok, "some json " <> str} # ... parse JSON
  @impl Parser
  def extensions, do: [".json"]
end

@spec parse_path(Path.t(), [module()]) :: {:ok, term} | {:error, atom}
def parse_path(filename, parsers) do
  with {:ok, ext} <- parse_extension(filename),
       {:ok, parser} <- find_parser(ext, parsers),
       {:ok, contents} <- File.read(filename) do
    parser.parse(contents)
  end
end

defp parse_extension(filename) do
  if ext = Path.extname(filename) do
    {:ok, ext}
  else
    {:error, :no_extension}
  end
end

defp find_parser(ext, parsers) do
  if parser = Enum.find(parsers, fn parser -> ext in parser.extensions() end) do
    {:ok, parser}
  else
    {:error, :no_matching_parser}
  end
end
