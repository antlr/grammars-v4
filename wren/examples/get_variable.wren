import "./get_variable_module"

class GetVariable {
  foreign static beforeDefined()
  foreign static afterDefined()
  foreign static afterAssigned()
  foreign static otherSlot()
  foreign static otherModule()
}

class Has {
  foreign static variable(module, variable)
  foreign static module(module)
}

System.print(GetVariable.beforeDefined()) // expect: null

var A = "a"

System.print(GetVariable.afterDefined()) // expect: a

A = "changed"

System.print(GetVariable.afterAssigned()) // expect: changed

var B = "b"
System.print(GetVariable.otherSlot()) // expect: b

System.print(GetVariable.otherModule()) // expect: value


System.print(Has.variable("./test/api/get_variable_module", "Variable")) // expect: true
System.print(Has.variable("./test/api/get_variable_module", "NotAVariable")) // expect: false
System.print(Has.variable("./test/api/get_variable", "Has")) // expect: true
System.print(Has.variable("./test/api/get_variable", "Fake")) // expect: false

System.print(Has.module("./test/api/get_variable_module")) // expect: true
System.print(Has.module("./test/api/get_variable")) // expect: true
System.print(Has.module("not a module")) // expect: false