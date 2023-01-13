import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;

public class Test {
	public static void main(String[] args) {
		String test =
//			"let arrayOfArrays: Array<Array<Int>> = []\n";
//			"var x = 2 > 5";
//			"x + -3";
//			"func increment() -> Void {\n"+
//				"        x!++\n"+
//				"        x!--\n"+
//				"        y++\n"+
//				"        y--\n"+
//				"        \n"+
//				"        // String interpolation\n"+
//				"        \"\\(bat)\\(man)\"\n"+
//				"    }";
			"    let testString: String = \"hey\"\n"+
				"    var subtractionTest = 100 - 5\n"+
				"    let multiplicationTest = 10 * 9\n"+
				"    var additionTest = 10 + 10\n"+
				"    var moduleTest = 10 % 10\n"+
				"    var gtTest = 5 > 1\n"+
				"    var ltTest = 5 < 10\n"+
				"    var not = !(5 > 10)\n"+
				"    var x: Int? = 0\n"+
				"    var y = 1\n"+
				"    var (z, _) = (10, 20)\n"+
				"    var (a, _, (b, c)) = (\"test\", 9.45, (12, 3))\n"+
				"    let bat = \"BAT\"\n"+
				"    let man = \"MAN\"\n";
		ANTLRInputStream input = new ANTLRInputStream(test);
		SwiftLexer lexer = new SwiftLexer(input);
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		tokens.fill();
		System.out.println(tokens.getTokens());
		SwiftParser parser = new SwiftParser(tokens);
		parser.setTrace(false);
		ParserRuleContext tree = parser.top_level();
		tree.inspect(parser);
	}
}
