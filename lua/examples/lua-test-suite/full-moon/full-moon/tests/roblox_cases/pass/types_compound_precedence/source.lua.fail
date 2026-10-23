-- https://github.com/Kampfkarren/full-moon/issues/286

-- should be parsed as a function returning a variable amount of values of type "string & T"
type FnA = () -> ...string & T

-- should be parsed as an intersection of a function returning U... values, and a value of type T
type FnB<U...> = () -> U... & T
