import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.stream.Collectors;

public class Symbol {
    private String name;
    private HashSet<TypeClassification> classification;
    private Map<String, Symbol> members = new HashMap<>();
    private Symbol parent;
    private boolean predefined = false;
    private String definedFile = "";
    private int definedLine = 0;
    private int definedColumn = 0;

    public String getDefinedFile() {
        return definedFile;
    }

    public void setDefinedFile(String definedFile) {
        this.definedFile = definedFile;
    }

    public int getDefinedLine() {
        return definedLine;
    }

    public void setDefinedLine(int definedLine) {
        this.definedLine = definedLine;
    }

    public int getDefinedColumn() {
        return definedColumn;
    }

    public void setDefinedColumn(int definedColumn) {
        this.definedColumn = definedColumn;
    }

    public boolean isPredefined() {
        return predefined;
    }

    public void setPredefined(boolean predefined) {
        this.predefined = predefined;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public HashSet<TypeClassification> getClassification() {
        return classification;
    }

    public void setClassification(HashSet<TypeClassification> classification) {
        this.classification = classification;
    }

    public Map<String, Symbol> getMembers() {
        return members;
    }

    public Symbol getParent() {
        return parent;
    }

    public void setParent(Symbol parent) {
        this.parent = parent;
    }

    @Override
    public String toString() {
        String result = name;
        String classificationStr = classification.stream()
            .map(TypeClassification::name)
            .collect(Collectors.joining(", "));
        result += " (with classification " + classificationStr + ")";
        if (definedFile != null && !definedFile.isEmpty()) {
            result += " defined at " + definedFile + ":" + definedLine + ":" + definedColumn;
        }
        return result;
    }
}
