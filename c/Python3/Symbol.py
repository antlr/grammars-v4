from TypeClassification import TypeClassification

class Symbol:
    def __init__(self):
        self.name = ""
        self.classification = set()
        self.members = dict()
        self.parent = None
        self.predefined = False
        self.definedFile = ""
        self.definedLine = 0
        self.definedColumn = 0

    def __str__(self):
        result = self.name
        classification_str = ", ".join(TypeClassification(c).name for c in self.classification)
        result += " (with classification " + classification_str + ")"
        if self.definedFile and len(self.definedFile) > 0:
            result += " defined at " + self.definedFile + ":" + str(self.definedLine) + ":" + str(self.definedColumn)
        return result
