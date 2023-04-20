import java.util.regex.Pattern.CharPredicate;
public class ExpressionOrder {
    public static void main(String[] args){
        Charpredicate a = func("a", true);
    }
    public static CharPredicate func(String name, boolean caseIns){
        return caseIns ? c -> Character.isUpperCase(c) ||
                                Character.isLowerCase(c) ||
                                Character.isTitleCase(c)
                        : Character::isUpperCase;;

    }

}
