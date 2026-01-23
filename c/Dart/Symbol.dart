import 'TypeClassification.dart';

class Symbol {
  String name = "";
  Set<TypeClassification> classification = {};
  Map<String, Symbol> members = {};
  Symbol? parent;
  bool predefined = false;
  String definedFile = "";
  int definedLine = 0;
  int definedColumn = 0;

  @override
  String toString() {
    var result = name;
    var classificationStr = classification.map((c) => c.name).join(", ");
    result += " (with classification $classificationStr)";
    if (definedFile.isNotEmpty) {
      result += " defined at $definedFile:$definedLine:$definedColumn";
    }
    return result;
  }
}
