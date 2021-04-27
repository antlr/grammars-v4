import java.util.*;

class HelloWorld {
    public static void main(String[] args) {
		int ctr = 0;

		Map<Integer, String> unicodeCategories = new HashMap<>();
		unicodeCategories.put((int)Character.UNASSIGNED, "Cn"); // 0
		unicodeCategories.put((int)Character.UPPERCASE_LETTER, "Lu"); // 1
		unicodeCategories.put((int)Character.LOWERCASE_LETTER, "Ll"); // 2
		unicodeCategories.put((int)Character.TITLECASE_LETTER, "Lt"); // 3
		unicodeCategories.put((int)Character.MODIFIER_LETTER, "Lm"); // 4
		unicodeCategories.put((int)Character.OTHER_LETTER, "Lo"); // 5
		unicodeCategories.put((int)Character.NON_SPACING_MARK, "Mn"); // 6
		unicodeCategories.put((int)Character.ENCLOSING_MARK, "Me"); // 7
		unicodeCategories.put((int)Character.COMBINING_SPACING_MARK, "Mc"); // 8
		unicodeCategories.put((int)Character.DECIMAL_DIGIT_NUMBER, "Nd"); // 9
		unicodeCategories.put((int)Character.LETTER_NUMBER, "Nl"); // 10
		unicodeCategories.put((int)Character.OTHER_NUMBER, "No"); // 11
		unicodeCategories.put((int)Character.SPACE_SEPARATOR, "Zs"); // 12
		unicodeCategories.put((int)Character.LINE_SEPARATOR, "Zl"); // 13
		unicodeCategories.put((int)Character.PARAGRAPH_SEPARATOR, "Zp"); // 14
		unicodeCategories.put((int)Character.CONTROL, "Cc"); // 15
		unicodeCategories.put((int)Character.FORMAT, "Cf"); // 16
		unicodeCategories.put((int)Character.PRIVATE_USE, "Co"); // 18
		unicodeCategories.put((int)Character.SURROGATE, "Cs"); // 19
		unicodeCategories.put((int)Character.DASH_PUNCTUATION, "Pd"); // 20
		unicodeCategories.put((int)Character.START_PUNCTUATION, "Ps"); // 21
		unicodeCategories.put((int)Character.END_PUNCTUATION, "Pe"); // 22
		unicodeCategories.put((int)Character.CONNECTOR_PUNCTUATION, "Pc"); // 23
		unicodeCategories.put((int)Character.OTHER_PUNCTUATION, "Po"); // 24
		unicodeCategories.put((int)Character.MATH_SYMBOL, "Sm"); // 25
		unicodeCategories.put((int)Character.CURRENCY_SYMBOL, "Sc"); // 26
		unicodeCategories.put((int)Character.MODIFIER_SYMBOL, "Sk"); // 27
		unicodeCategories.put((int)Character.OTHER_SYMBOL, "So"); // 28
		unicodeCategories.put((int)Character.INITIAL_QUOTE_PUNCTUATION, "Pi"); // 29
		unicodeCategories.put((int)Character.FINAL_QUOTE_PUNCTUATION, "Pf"); // 30

		Set<Integer> full_category = new HashSet<Integer>();
		
		for (int category = 0; category < 31; ++category)
		{
			// Verify every code point in category is also in isJavaIdentiferStart().
			boolean all = true;
			boolean any = false;
			for (int codePoint = 0; codePoint < 0x10FFFF; codePoint++)
			{
				var general_class = Character.getType(codePoint);
				if (category == general_class)
				{
					any = true;
					if (! Character.isJavaIdentifierPart(codePoint))
					{
						all = false;
						break;
					}
				}
			}
			if (! all)
			{
				continue;
			}
			if (! any)
			{
				continue;
			}
			var name = unicodeCategories.get(category);
			if (name == null)
			{
				continue;
			}
			System.out.println(name);
			full_category.add(category);
		}

		// Now go through all code points and verify that it appears in one full category.
		// Otherwise, we need to now add that to the special case list.
		for (int codePoint = 0; codePoint < 0x10FFFF; codePoint++)
		{
			if (! Character.isJavaIdentifierPart(codePoint))
			{
				continue;
			}
			var general_class = Character.getType(codePoint);
			if (full_category.contains(general_class))
			{
				continue;
			}
			System.out.println(String.format("Plus 0x%08X", codePoint));
		}

		//System.out.println("Number of UC codepoints " + ctr);
	}
}
