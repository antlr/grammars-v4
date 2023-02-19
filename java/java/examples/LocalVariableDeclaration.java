import java.util.ArrayList;
public class LocalVariableDeclaration {
    public static void main(String[] args)
    {
        ArrayList<Integer> myList = new ArrayList<Integer>();
        myList.add(2);
        var myVal = myList.get(0);
    }
}
