
public class Escapes{

    void f(){
        String s1 = "-\u005ct-"; // -    -
        String s2 = "-\u005c"-"; // -"-
        String s3 = "-\u005c\-"; // -\-
        String s4 = "-\u005c'-"; // -'-
        String s5 = "\u005c"";   // -"-

        String s6 = "\u005c101"; //A
        String s7 = "\u005c043"; //#
        String s8 = "\u005c0"; // nul

        char c1 = '\u005cn'; // \n
        char c2 = '\u005c''; // \'
        char c3 = '\u005c"'; // \"
        char c4 = '\u005c\'; // \\
    }
}
