import org.antlr.v4.runtime.*;

import java.io.FileInputStream;
import java.io.InputStream;

public class TestR {
	public static void main(String[] args) throws Exception {
		String inputFile = null;
		if ( args.length>0 ) inputFile = args[0];
		InputStream is = System.in;
		if ( inputFile!=null ) {
			is = new FileInputStream(inputFile);
		}
		ANTLRInputStream input = new ANTLRInputStream(is);
		RLexer lexer = new RLexer(input);
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		// Print tokens BEFORE filtering
//		tokens.fill();
//		for (Object tok : tokens.getTokens()) {
//			System.out.println(tok);
//		}
		RFilter filter = new RFilter(tokens);
		filter.stream(); // call start rule: stream
		tokens.reset();
		// Print tokens AFTER filtering
//		for (Object tok : tokens.getTokens()) {
//			System.out.println(tok);
//		}
		RParser parser = new RParser(tokens);
		parser.setBuildParseTree(true);
		RuleContext tree = parser.prog();
		//tree.save(parser, "/tmp/R.ps"); // Generate postscript
		System.out.println(tree.toStringTree(parser));
	}
}
