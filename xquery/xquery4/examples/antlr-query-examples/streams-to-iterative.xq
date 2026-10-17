
(: import grammar antlr = "grammars/Java20Parser.g4", "grammars/Java20Lexer.g4"; :)
(: declare default element namespace antlr; :)
(:
Example:
    List<Range> positions = extractionInfo.contextStack.stream()    | Set<Range> insertedRanges = new HashSet<>(extractionInfo.contextStack.size());
        .map(ctx->getContextRange(ctx))                             | List<Range> ranges = new ArrayList<>(extractionInfo.contextStack.size());
        .distinct()                                                 | for (var ctx : extractionInfo.contextStack) {
        .toList();                                                  |     var range = getContextRange(ctx);
                                                                    |     if (insertedRanges.add(range)) {
                                                                    |         ranges.add(range);
                                                                    |     }
                                                                    | }
:)

(: terminating functions
boolean	allMatch(Predicate<? super T> predicate)
Returns whether all elements of this stream match the provided predicate.
boolean	anyMatch(Predicate<? super T> predicate)
Returns whether any elements of this stream match the provided predicate.
<R,A> R	collect(Collector<? super T,A,R> collector)
Performs a mutable reduction operation on the elements of this stream using a Collector.
<R> R	collect(Supplier<R> supplier, BiConsumer<R,? super T> accumulator, BiConsumer<R,R> combiner)
Performs a mutable reduction operation on the elements of this stream.
long	count()
void	forEach(Consumer<? super T> action)
Performs an action for each element of this stream.
void	forEachOrdered(Consumer<? super T> action)
Performs an action for each element of this stream, in the encounter order of the stream if the stream has a defined encounter order.
Optional<T>	findFirst()
Returns an Optional describing the first element of this stream, or an empty Optional if the stream is empty.
Optional<T>	findAny()
Returns an Optional describing some element of the stream, or an empty Optional if the stream is empty.
Optional<T>	max(Comparator<? super T> comparator)
Returns the maximum element of this stream according to the provided Comparator.
Optional<T>	min(Comparator<? super T> comparator)
Returns the minimum element of this stream according to the provided Comparator.
Optional<T>	reduce(BinaryOperator<T> accumulator)
Performs a reduction on the elements of this stream, using an associative accumulation function, and returns an Optional describing the reduced value, if any.
T	reduce(T identity, BinaryOperator<T> accumulator)
Performs a reduction on the elements of this stream, using the provided identity value and an associative accumulation function, and returns the reduced value.
<U> U	reduce(U identity, BiFunction<U,? super T,U> accumulator, BinaryOperator<U> combiner)
Performs a reduction on the elements of this stream, using the provided identity, accumulation and combining functions.
Object[]	toArray()
Returns an array containing the elements of this stream.
<A> A[]	toArray(IntFunction<A[]> generator)
Returns an array containing the elements of this stream, using the provided generator function to allocate the returned array, as well as any additional arrays that might be required for a partitioned execution or for resizing.
:)

(:
static <T> Stream.Builder<T>	builder()
Returns a builder for a Stream.
static <T> Stream<T>	concat(Stream<? extends T> a, Stream<? extends T> b)
Creates a lazily concatenated stream whose elements are all the elements of the first stream followed by all the elements of the second stream.
Returns the count of elements in this stream.
Stream<T>	distinct()
Returns a stream consisting of the distinct elements (according to Object.equals(Object)) of this stream.
static <T> Stream<T>	empty()
Returns an empty sequential Stream.
Stream<T>	filter(Predicate<? super T> predicate)
Returns a stream consisting of the elements of this stream that match the given predicate.
<R> Stream<R>	flatMap(Function<? super T,? extends Stream<? extends R>> mapper)
Returns a stream consisting of the results of replacing each element of this stream with the contents of a mapped stream produced by applying the provided mapping function to each element.
DoubleStream	flatMapToDouble(Function<? super T,? extends DoubleStream> mapper)
Returns an DoubleStream consisting of the results of replacing each element of this stream with the contents of a mapped stream produced by applying the provided mapping function to each element.
IntStream	flatMapToInt(Function<? super T,? extends IntStream> mapper)
Returns an IntStream consisting of the results of replacing each element of this stream with the contents of a mapped stream produced by applying the provided mapping function to each element.
LongStream	flatMapToLong(Function<? super T,? extends LongStream> mapper)
Returns an LongStream consisting of the results of replacing each element of this stream with the contents of a mapped stream produced by applying the provided mapping function to each element.
static <T> Stream<T>	generate(Supplier<T> s)
Returns an infinite sequential unordered stream where each element is generated by the provided Supplier.
static <T> Stream<T>	iterate(T seed, UnaryOperator<T> f)
Returns an infinite sequential ordered Stream produced by iterative application of a function f to an initial element seed, producing a Stream consisting of seed, f(seed), f(f(seed)), etc.
Stream<T>	limit(long maxSize)
Returns a stream consisting of the elements of this stream, truncated to be no longer than maxSize in length.
<R> Stream<R>	map(Function<? super T,? extends R> mapper)
Returns a stream consisting of the results of applying the given function to the elements of this stream.
DoubleStream	mapToDouble(ToDoubleFunction<? super T> mapper)
Returns a DoubleStream consisting of the results of applying the given function to the elements of this stream.
IntStream	mapToInt(ToIntFunction<? super T> mapper)
Returns an IntStream consisting of the results of applying the given function to the elements of this stream.
LongStream	mapToLong(ToLongFunction<? super T> mapper)
Returns a LongStream consisting of the results of applying the given function to the elements of this stream.
boolean	noneMatch(Predicate<? super T> predicate)
Returns whether no elements of this stream match the provided predicate.
static <T> Stream<T>	of(T... values)
Returns a sequential ordered stream whose elements are the specified values.
static <T> Stream<T>	of(T t)
Returns a sequential Stream containing a single element.
Stream<T>	peek(Consumer<? super T> action)
Returns a stream consisting of the elements of this stream, additionally performing the provided action on each element as elements are consumed from the resulting stream.
Stream<T>	skip(long n)
Returns a stream consisting of the remaining elements of this stream after discarding the first n elements of the stream.
Stream<T>	sorted()
Returns a stream consisting of the elements of this stream, sorted according to natural order.
Stream<T>	sorted(Comparator<? super T> comparator)
Returns a stream consisting of the elements of this stream, sorted according to the provided Comparator.
:)


declare type method-call as element(primaryNoNewArray | pNNA | methodInvocation);
declare function get-method-calls($node as node()* := .) as method-call* {
    $node//(methodInvocation|primaryNoNewArray|pNNA)[./LPAREN]
};


let $method-calls as method-call* := get-method-calls()
for sliding window $stream-calls in $method-calls
    start previous $stream-init when $stream-init/identifier/string() = "stream" (: TODO: parallel stream :)
    end next $stream-end when $stream-end/(identifier|methodName)/string() = (
        "allMatch", "anyMatch", "collect", "count", "forEach", "forEachOrdered",
        "findFirst", "findAny", "max", "min", "reduce", "toArray"
        )
    let $following-calls := $stream-end//(methodInvocation|primaryNoNewArray|pNNA) (: TODO: relative to ';':)
    let $processed := ( for $call in $stream-calls return $call )
    return switch (string($stream-end/(methodName|identifier)))
    {
        case "allMatch" return ()
        case "allMatch" return ()
        case "anyMatch" return ()
        case "collect" return ()
        case "count" return ()
        case "forEach" return ()
        case "forEachOrdered" return ()
        case "findFirst" return ()
        case "findAny" return ()
        case "max" return ()
        case "min" return ()
        case "reduce" return ()
        default (:toArray:) return ()
    }
