// Covers: refinement, refineDcl (VAL valDcl, DEF defDcl, VAR valDcl, TYPE id typeBounds),
//         valDcl, defDcl, contextBounds LBRACE form, contextBound AS id

// Structural type with refinement: val, def, var, type
type HasName = { val name: String }
type HasMutable = { var count: Int }
type HasMethod = { def greet(x: String): String }
type HasType = { type T <: AnyRef }
type HasAll = { val name: String; var count: Int; def run(): Unit; type Elem }

// Multiple refinements
type Complex = AnyRef { val id: Int; def describe(): String }

// Using structural type as parameter
def withName(x: { val name: String }): String = x.name
def withMethod(x: { def run(): Unit }): Unit = x.run()

// Refinement with type bounds in refineDcl
type Bounded = AnyRef { type T <: Number }

// Context bounds with LBRACE form (multiple context bounds)
def multiCtx[T: { Ordering }](x: T, y: T): T = x

// contextBound with AS id
def namedCtx[T: Ordering as ord](x: T, y: T): Int = ord.compare(x, y)

object StructuralDemo {
  val named: { val name: String } = new { val name = "Alice" }
  val result = withName(named)
}
