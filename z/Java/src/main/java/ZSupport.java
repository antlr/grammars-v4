import java.util.HashSet;
import java.util.Set;

import org.antlr.v4.runtime.TokenStream;

public class ZSupport {
	public static Set<String> rightAssociativity = new HashSet<String>();
	
	static boolean isLeftAssociative(TokenStream tokens) {
		return !rightAssociativity.contains(tokens.get(tokens.index()).getText());
	}
}
