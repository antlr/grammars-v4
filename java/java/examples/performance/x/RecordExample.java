import java.util.Arrays;

public class RecordExample {

    // Record declaration
    public record Person(String name, int ... age) { }

    public static void main(String[] args) {
	// Creating a new Person record
	Person person = new Person("Alice", 30, 20, 10);

	// Accessing fields
	System.out.println("Name: " + person.name());
	System.out.println("Age: " + Arrays.toString(person.age()));

	// toString(), equals(), and hashCode() are automatically implemented
	Person person2 = new Person("Alice", 30);
	System.out.println("Equals: " + person.equals(person2));
	System.out.println("ToString: " + person);
    }
}
