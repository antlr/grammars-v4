module ModAlias = ModuleName;
deprecated module OldVersion = NewVersion;
class TypeAlias = TypeName;
class Bool = boolean;
import OneTwoThreeLib

class OT = M::OneTwo;


from OT ot
select ot

int getSuccessor(int i) {
  result = i + 1 and
  i in [1 .. 9]
}

predicate succ = getSuccessor/1;

predicate isSmall(int i) {
  i in [1 .. 9]
}
predicate lessThanTen = isSmall/1;

