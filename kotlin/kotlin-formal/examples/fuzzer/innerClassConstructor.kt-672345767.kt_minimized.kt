fun box(): String {
return (((A::Nested)!!))!!.call("O").result + (A::Inner).call((::A).call(), "K").result
}