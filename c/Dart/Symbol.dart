import 'TypeClassification.dart';

class Symbol {
  String name = "";
  Set<TypeClassification> classification = {};
  Map<String, Symbol> members = {};
  Symbol? parent;

  @override
  String toString() {
    var result = name;
    var classificationStr = classification.map((c) => c.name).join(", ");
    result += " (with classification $classificationStr)";
    return result;
  }
}
