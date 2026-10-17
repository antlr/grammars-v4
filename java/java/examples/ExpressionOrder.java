import java.util.regex.Pattern.CharPredicate;
public class ExpressionOrder {
    public static void main(String[] args){
        Charpredicate a = func("a", true);
    }
    // Test that method reference takes precedence over ?: syntax
    public static CharPredicate func(String name, boolean caseIns){
        return caseIns ? c -> Character.isUpperCase(c) ||
                                Character.isLowerCase(c) ||
                                Character.isTitleCase(c)
                        : Character::isUpperCase;;

    }

    // Test that method reference takes precedence over casting
    void forEachRemaining(Consumer<? super Double> action) {
            forEachRemaining((DoubleConsumer) action::accept);
    }
}
