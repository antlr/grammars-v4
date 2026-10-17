// Covers: typTypeParamClause TLARROW (type lambda), QUESTION typeBounds (wildcard type),
//         singleton DOT TYPE, singleton DOT id (type selection), simpleType_ HASH id (type projection),
//         nameAndType (named tuple type), funParamClause + typedFunParam,
//         funArgType ARROW type_ (by-name), singleton (DOT id chain)

// Type lambda: [T] =>> F[T]
type MyList = [T] =>> List[T]
type MyMap  = [K, V] =>> Map[K, V]

// Wildcard types
def acceptWild(x: List[?]): Unit = ()
def acceptBounded(x: List[? <: AnyRef]): Unit = ()
def acceptLower(x: List[? >: String]): Unit = ()

// Singleton type: x.type
def identity(x: String): x.type = x

// Type selection on singleton: Obj.Type
object TypeContainer {
  type Inner = Int
  type Pair = (Int, String)
}
def useInner(x: TypeContainer.Inner): TypeContainer.Inner = x

// Type projection: Outer#Inner
class OuterClass {
  type InnerType = String
}
type Projected = OuterClass#InnerType

// Named tuple type: (name: Type, name2: Type)
type NamedPair = (first: Int, second: String)
def makeNamed(f: Int, s: String): (first: Int, second: String) = (f, s)

// Dependent function type with funParamClause + typedFunParam
type DepId = (x: Int) => x.type

// By-name function arg type: => Type
type ByNameFun = (=> Int) => Int
def acceptByName(f: => Int): Int = f

// Singleton chain: x.y.z
object Outer {
  object Inner {
    type T = Int
  }
}
def chainedType(x: Outer.Inner.T): Outer.Inner.T = x

// typTypeParamClause ARROW type_ (type-lambda in funType position)
type TypeFun = [T] => List[T]
