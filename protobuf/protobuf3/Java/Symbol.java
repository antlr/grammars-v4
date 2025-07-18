import java.util.HashMap;
import java.util.Map;

public class Symbol {
    private String name;
    private TypeClassification classification;
    private final Map<String, Symbol> members = new HashMap<>();
    private Symbol parent;

    public Symbol()
    {
    }
    
    public Symbol(String name, TypeClassification classification)
    {
        this.name = name;
        this.classification = classification;
    }
    
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public TypeClassification getClassification() {
        return classification;
    }

    public void setClassification(TypeClassification classification) {
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
        StringBuilder result = new StringBuilder();
        result.append(name);
        String classificationStr = classification != null ? classification.toString() : "null";
        result.append(" (with classification ").append(classificationStr);
        result.append(")");
        if (parent != null) {
            result.append(" of ").append(parent.toString());
        }
        return result.toString();
    }
}
