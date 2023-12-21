
class Person extends string{
  Person(){
    this = "asdf"
  }
}
bindingset[a]
predicate foo(int a) {
  "Ann" < "Anne"
  and 5 + 6 >= 11
  and  4 != 5
  and not a = 3
  and 1 != [1 .. 2]

  and a in [2.1 .. 10.5]
  and (a in [2.1 .. 10.5])
  and exists(int i | i instanceof SmallInt)
  and forall(int i | i instanceof SmallInt | i < 5)
}

string visibility(Class c){
  if c.isPublic()
  then result = "public"
  else result = "private"
}

class SmallInt extends int {
  SmallInt() { this = [1 .. 10] }
}

from SmallInt x
where x % 2 = 0 implies x % 4 = 0
select x
