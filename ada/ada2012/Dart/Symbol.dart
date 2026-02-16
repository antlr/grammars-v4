import 'TypeClassification.dart';

class Symbol {
  String name = "";
  Set<TypeClassification> classification = {};
  Map<String, Symbol> members = {};
  Symbol? parent;
  bool predefined = false;
  bool isComposite = false;
  String definedFile = "";
  int definedLine = 0;
  int definedColumn = 0;

  @override
  String toString() {
    var result = name;
    result += " (with classification ${classification.map((c) => c.name).join(', ')})";
    if (isComposite) result += " [composite]";
    if (definedFile.isNotEmpty) result += " defined at $definedFile:$definedLine:$definedColumn";
    return result;
  }
}
