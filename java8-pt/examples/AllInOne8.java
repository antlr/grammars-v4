// Source: https://en.wikipedia.org/wiki/Java_syntax

/* This is a multi-line comment.
It may occupy more than one line. */

// This is an end-of-line comment

/**
 * This is a documentation comment.
 * 
 * @author John Doe
 */
 
package myapplication.mylibrary;

import java.util.Random; // Single type declaration
import java.util.*;
import java.*;
import static java.lang.System.out; //'out' is a static field in java.lang.System
import static screen.ColorName.*;

public enum ColorName {
    RED, BLUE, GREEN
};

public class ImportsTest {
    public static void main(String[] args) {
        /* The following line is equivalent to
         * java.util.Random random = new java.util.Random();
         * It would've been incorrect without the import declaration */
        Random random = new Random();
    }
}

public class HelloWorld {
    public static void main(String[] args) {
        /* The following line is equivalent to:
           System.out.println("Hello World!");
           and would have been incorrect without the import declaration. */
        out.println("Hello World!");
        
        // Conditional statements -------------------
        if (i == 3) doSomething();
        
        if (i == 2) {
            doSomething();
        } else {
            doSomethingElse();
        }

        if (i == 3) {
            doSomething();
        } else if (i == 2) {
            doSomethingElse();
        } else {
            doSomethingDifferent();
        }
        
        int a = 1;
        int b = 2;
        int minVal = (a < b) ? a : b;
        
        // switch
        switch (ch) {
            case 'A':
                doSomething(); // Triggered if ch == 'A'
                break;
            case 'B':
            case 'C':
                doSomethingElse(); // Triggered if ch == 'B' or ch == 'C'
                break;
            default:
                doSomethingDifferent(); // Triggered in any other case
                break;
        }
        
        // Iteration statements -------------------
        while (i < 10) {
            doSomething();
        }
        
        do {
            doSomething();
        } while (i < 10);
        
        for (int i = 0; i < 10; i++) {
            doSomething();
        }
        
        // A more complex loop using two variables
        for (int i = 0, j = 9; i < 10; i++, j -= 3) {
            doSomething();
        }
        
        for (;;) {
            doSomething();
        }
        
        for (int i : intArray) {
            doSomething(i);
        }
        
        // Jump statements -------------------
        // Label
        start:
            someMethod();
        
        // break
        for (int i = 0; i < 10; i++) {
            while (true) {
                break;
            }
            // Will break to this point
        }
        
        outer:
            for (int i = 0; i < 10; i++) {
                while (true) {
                    break outer;
                }
            }
            // Will break to this point

        // continue
        int ch;
        while (ch = getChar()) {
            if (ch == ' ') {
                continue; // Skips the rest of the while-loop
            }

            // Rest of the while-loop, will not be reached if ch == ' '
            doSomething();
        }
        
        outer:
        for (String str : stringsArr) {
            char[] strChars = str.toCharArray();
            for (char ch : strChars) {
                if (ch == ' ') {
                    /* Continues the outer cycle and the next
                    string is retrieved from stringsArr */
                    continue outer;
                }
                doSomething(ch);
            }
        }
        
        // return
        // If streamClosed is true, execution is stopped
        if (streamClosed) {
            return;
        }
        readFromStream();

        int result = a + b;
        return result;
        
        // Exception handling statements -------------------
        // try-catch-finally
        try {
            // Statements that may throw exceptions
            methodThrowingExceptions();
        } catch (Exception ex) {
            // Exception caught and handled here
            reportException(ex);
        } finally {
            // Statements always executed after the try/catch blocks
            freeResources();
        }
        
        try {
            methodThrowingExceptions();
        } catch (IOException | IllegalArgumentException ex) {
            //Both IOException and IllegalArgumentException will be caught and handled here
            reportException(ex);
        }
        
        // try-with-resources statement
        try (FileOutputStream fos = new FileOutputStream("filename");
            XMLEncoder xEnc = new XMLEncoder(fos))
        {
            xEnc.writeObject(object);
        } catch (IOException ex) {
            Logger.getLogger(Serializer.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        // throw
        if (obj == null) {
            // Throws exception of NullPointerException type
            throw new NullPointerException();
        }
        // Will not be called, if object is null
        doSomethingWithObject(obj);
        
        // Thread concurrency control -------------------
        /* Acquires lock on someObject. It must be of a reference type and must be non-null */
        synchronized (someObject) {
            // Synchronized statements
        }
        
        // assert statement
        // If n equals 0, AssertionError is thrown
        assert n != 0;
        /* If n equals 0, AssertionError will be thrown
        with the message after the colon */
        assert n != 0 : "n was equal to zero";
        
        // Reference types -------------------
        // Arrays
        int[] numbers = new int[5];
        numbers[0] = 2;
        int x = numbers[0];
        
        // Initializers -------------------
        // Long syntax
        int[] numbers = new int[] {20, 1, 42, 15, 34};
        // Short syntax
        int[] numbers2 = {20, 1, 42, 15, 34};
        
        // Multi-dimensional arrays
        int[][] numbers = new int[3][3];
        numbers[1][2] = 2;
        int[][] numbers2 = {{2, 3, 2}, {1, 2, 6}, {2, 4, 5}};
        
        int[][] numbers = new int[2][]; //Initialization of the first dimension only
        numbers[0] = new int[3];
        numbers[1] = new int[2];
    }
}

// Classes -------------------
// Top-level class 
class Foo {
    // Class members
}

// Inner class
class Foo { // Top-level class
    class Bar { // Inner class
    }
}

// Nested class
class Foo { // Top-level class
    static class Bar { // Nested class
    }
}

// Local class
class Foo {
    void bar() {
        class Foobar {// Local class within a method
        }
    }
}

// Anonymous class
class Foo {
    void bar() {
        new Object() {// Creation of a new anonymous class extending Object
        };
    }
}

// Access modifiers
public class Foo {
    int go() {
        return 0;
    }

    private class Bar {
    }
}

// Constructors and initializers
class Foo {
    String str;

    Foo() { // Constructor with no arguments
        // Initialization
    }

    Foo(String str) { // Constructor with one argument
        this.str = str;
    }
}

class Foo {
    static {
        // Initialization
    }
}

class Foo {
    {
        // Initialization
    }
}

// Methods -------------------
class Foo {
    int bar(int a, int b) {
        return (a*2) + b;
    }

    /* Overloaded method with the same name but different set of arguments */
    int bar(int a) {
        return a*2;
    }
    
    void openStream() throws IOException, myException { // Indicates that IOException may be thrown
    }
    
    // Varargs
    void printReport(String header, int... numbers) { //numbers represents varargs
        System.out.println(header);
        for (int num : numbers) {
            System.out.println(num);
        }
    }
}

// Overriding methods
class Operation {
    public int doSomething() {
        return 0;
    }
}

class NewOperation extends Operation {
    @Override
    public int doSomething() {
        return 1;
    }
}

// Abstract classes
public class AbstractClass {
    private static final String hello;

    static {
        System.out.println(AbstractClass.class.getName() + ": static block runtime");
        hello = "hello from " + AbstractClass.class.getName();
    }

    {
        System.out.println(AbstractClass.class.getName() + ": instance block runtime");
    }

    public AbstractClass() {
        System.out.println(AbstractClass.class.getName() + ": constructor runtime");
    }

    public static void hello() {
        System.out.println(hello);
    }
}

public class CustomClass extends AbstractClass {

    static {
        System.out.println(CustomClass.class.getName() + ": static block runtime");
    }

    {
        System.out.println(CustomClass.class.getName() + ": instance block runtime");
    }

    public CustomClass() {
        System.out.println(CustomClass.class.getName() + ": constructor runtime");
    }

    public static void main(String[] args) {
        CustomClass nc = new CustomClass();
        hello();
        //AbstractClass.hello();//also valid
    }
}

// Enumerations -------------------
enum Season {
    WINTER, SPRING, SUMMER, AUTUMN
}

public enum Season {
    WINTER("Cold"), SPRING("Warmer"), SUMMER("Hot"), AUTUMN("Cooler");

    Season(String description) {
        this.description = description;
    }

    private final String description;

    public String getDescription() {
        return description;
    }
}

public enum Season {
    WINTER {
        String getDescription() {return "cold";}
    },
    SPRING {
        String getDescription() {return "warmer";}
    },
    SUMMER {
        String getDescription() {return "hot";}
    },
    FALL {
        String getDescription() {return "cooler";}
    };
}

// Interfaces -------------------
interface ActionListener {
    int ACTION_ADD = 0;
    int ACTION_REMOVE = 1;
 
    void actionSelected(int action);
}

interface RequestListener {
    int requestReceived();
}

class ActionHandler implements ActionListener, RequestListener {
    public void actionSelected(int action) {
    }

    public int requestReceived() {
    }
}

class Dummy {
    public void dummy() {
        //Calling method defined by interface
        RequestListener listener = new ActionHandler(); /*ActionHandler can be
                                           represented as RequestListener...*/
        listener.requestReceived(); /*...and thus is known to implement
                                    requestReceived() method*/
    }
}

// Annotations  -------------------
@interface BlockingOperations {
}

@interface BlockingOperations {
    boolean fileSystemOperations();
    boolean networkOperations() default false;
}

class Dummy {
    @BlockingOperations(/*mandatory*/ fileSystemOperations = true,
    /*optional*/ networkOperations = true)
    void openOutputStream() { //Annotated method
    }

    @Unused // Shorthand for @Unused()
    void travelToJupiter() {
    }
}

// Generics -------------------
// Generic classes
/* This class has two type variables, T and V. T must be 
a subtype of ArrayList and implement Formattable interface */
public class Mapper<T extends ArrayList & Formattable, V> {
    public void add(T array, V item) {
        // array has add method because it is an ArrayList subclass
        array.add(item);
        
        /* Mapper is created with CustomList as T and Integer as V.
        CustomList must be a subclass of ArrayList and implement Formattable */
        Mapper<CustomList, Integer> mapper = new Mapper<CustomList, Integer>();
        
        Mapper<CustomList, ?> mapper;
        mapper = new Mapper<CustomList, Boolean>();
        mapper = new Mapper<CustomList, Integer>();
    }
}

// Generic methods and constructors -------------------
class Mapper {
    // The class itself is not generic, the constructor is
    <T, V> Mapper(T array, V item) {
    }
    
    /* This method will accept only arrays of the same type as
    the searched item type or its subtype*/
    static <T, V extends T> boolean contains(T item, V[] arr) {
        for (T currentItem : arr) {
            if (item.equals(currentItem)) {
                return true;
            }
        }
        return false;
    }
}

interface Expandable<T extends Number> {
    void addItem(T item);
}

// This class is parameterized
class Array<T extends Number> implements Expandable<T> {
    void addItem(T item) {
    }
}

// And this is not and uses an explicit type instead
class IntegerArray implements Expandable<Integer> {
    void addItem(Integer item) {
    }
}

// Lambdas
public class Lambdas {
     public static void main(String[] args) {
        // use predicate composition to remove matching names
        List<Name> list = new ArrayList<>();
        for (Name name : NAMES) {
            list.add(name);
        }
        Predicate<Name> pred1 = name -> "Sally".equals(name.firstName);
        Predicate<Name> pred2 = name -> "Queue".equals(name.lastName);
        list.removeIf(pred1.or(pred2));
        printNames("Names filtered by predicate:", list.toArray(new Name[list.size()]));

        Comparator<Name> com1 = Comparator.comparing((Name name1) -> name1.lastName)
            .thenComparing(name2 -> name2.firstName);
        Comparator<Name> com2 = Comparator.<Name,String>comparing(name1 -> name1.lastName)
            .thenComparing(name2 -> name2.firstName);

        // sort array using lambda expression
        copy = Arrays.copyOf(NAMES, NAMES.length);
        Arrays.sort(copy, (a, b) -> a.compareTo(b));
        printNames("Names sorted with lambda expression:", copy);
     }
}
