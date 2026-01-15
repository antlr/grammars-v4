import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.stream.Collectors;

public class Symbol {
    private String name;
    private HashSet<TypeClassification> classification;
    private Map<String, Symbol> members = new HashMap<>();
    private Symbol parent;

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
        return result;
    }
}
