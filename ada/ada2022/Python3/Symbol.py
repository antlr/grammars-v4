class Symbol:
    def __init__(self):
        self.name = ""
        self.classification = set()
        self.members = {}
        self.parent = None
        self.predefined = False
        self.is_composite = False
        self.defined_file = ""
        self.defined_line = 0
        self.defined_column = 0

    def __str__(self):
        result = self.name
        result += " (with classification " + ", ".join(str(c) for c in self.classification) + ")"
        if self.is_composite:
            result += " [composite]"
        if self.defined_file:
            result += f" defined at {self.defined_file}:{self.defined_line}:{self.defined_column}"
        return result
