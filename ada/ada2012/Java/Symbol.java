import java.util.*;

public class Symbol {
    private String name;
    private Set<TypeClassification> classification;
    private Map<String, Symbol> members = new LinkedHashMap<>();
    private Symbol parent;
    private boolean predefined = false;
    private boolean isComposite = false;
    private String definedFile = "";
    private int definedLine = 0;
    private int definedColumn = 0;

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    public Set<TypeClassification> getClassification() { return classification; }
    public void setClassification(Set<TypeClassification> classification) { this.classification = classification; }
    public Map<String, Symbol> getMembers() { return members; }
    public Symbol getParent() { return parent; }
    public void setParent(Symbol parent) { this.parent = parent; }
    public boolean isPredefined() { return predefined; }
    public void setPredefined(boolean predefined) { this.predefined = predefined; }
    public boolean isComposite() { return isComposite; }
    public void setComposite(boolean isComposite) { this.isComposite = isComposite; }
    public String getDefinedFile() { return definedFile; }
    public void setDefinedFile(String definedFile) { this.definedFile = definedFile; }
    public int getDefinedLine() { return definedLine; }
    public void setDefinedLine(int definedLine) { this.definedLine = definedLine; }
    public int getDefinedColumn() { return definedColumn; }
    public void setDefinedColumn(int definedColumn) { this.definedColumn = definedColumn; }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder(name);
        result.append(" (with classification ");
        StringJoiner joiner = new StringJoiner(", ");
        for (TypeClassification tc : classification) {
            joiner.add(tc.toString());
        }
        result.append(joiner.toString()).append(")");
        if (isComposite) {
            result.append(" [composite]");
        }
        if (definedFile != null && !definedFile.isEmpty()) {
            result.append(" defined at ").append(definedFile).append(":").append(definedLine).append(":").append(definedColumn);
        }
        return result.toString();
    }
}
