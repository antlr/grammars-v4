import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.WritableToken;
import org.antlr.v4.runtime.tree.TerminalNode;

public class ZOperatorListener extends ZOperatorParserBaseListener {
	public Map<String, Integer> associations = new HashMap<String, Integer>();
	public Set<String> rightAssociativity = new HashSet<String>();
	private boolean isRightAssoc = false;
	
	private static final Map<Integer, Integer> relationMap = createRMap();

	private static Map<Integer, Integer> createRMap() {
		Map<Integer, Integer> myMap = new HashMap<Integer, Integer>(11);
		myMap.put(ZLexer.PRE, ZLexer.PREP);
		myMap.put(ZLexer.POST, ZLexer.POSTP);
		myMap.put(ZLexer.I, ZLexer.IP);
		myMap.put(ZLexer.L, ZLexer.LP);
		myMap.put(ZLexer.EL, ZLexer.ELP);
		myMap.put(ZLexer.ER, ZLexer.ERP);
		myMap.put(ZLexer.SR, ZLexer.SRP);
		myMap.put(ZLexer.ERE, ZLexer.EREP);
		myMap.put(ZLexer.SRE, ZLexer.SREP);
		myMap.put(ZLexer.ES, ZLexer.ES);
		myMap.put(ZLexer.SS, ZLexer.SS);
		return myMap;
	}

	private static final Map<Integer, Integer> functionMap = createFMap();

	private static Map<Integer, Integer> createFMap() {
		Map<Integer, Integer> myMap = new HashMap<Integer, Integer>(11);
		myMap.put(ZLexer.PRE, ZLexer.PRE);
		myMap.put(ZLexer.POST, ZLexer.POST);
		myMap.put(ZLexer.I, ZLexer.I);
		myMap.put(ZLexer.L, ZLexer.L);
		myMap.put(ZLexer.EL, ZLexer.EL);
		myMap.put(ZLexer.ER, ZLexer.ER);
		myMap.put(ZLexer.SR, ZLexer.SR);
		myMap.put(ZLexer.ERE, ZLexer.ERE);
		myMap.put(ZLexer.SRE, ZLexer.SRE);
		myMap.put(ZLexer.ES, ZLexer.ES);
		myMap.put(ZLexer.SS, ZLexer.SS);
		return myMap;
	}
	
	private static final Map<Integer, Integer> prefixMap = createPreMap();

	private static Map<Integer, Integer> createPreMap() {
		Map<Integer, Integer> myMap = new HashMap<Integer, Integer>(4);
		myMap.put(ZLexer.I, ZLexer.PRE);
		myMap.put(ZLexer.L, ZLexer.L);
		myMap.put(ZLexer.ER, ZLexer.ERE);
		myMap.put(ZLexer.SR, ZLexer.SRE);
		return myMap;
	}
	
	private static final Map<Integer, Integer> postfixMap = createPostMap();

	private static Map<Integer, Integer> createPostMap() {
		Map<Integer, Integer> myMap = new HashMap<Integer, Integer>(4);
		myMap.put(ZLexer.I, ZLexer.POST);
		myMap.put(ZLexer.L, ZLexer.EL);
		myMap.put(ZLexer.ER, ZLexer.ER);
		myMap.put(ZLexer.SR, ZLexer.SR);
		return myMap;
	}
	
	private static final Map<Integer, Integer> infixMap = createInfixMap();

	private static Map<Integer, Integer> createInfixMap() {
		Map<Integer, Integer> myMap = new HashMap<Integer, Integer>(4);
		myMap.put(ZLexer.I, ZLexer.I);
		myMap.put(ZLexer.L, ZLexer.EL);
		myMap.put(ZLexer.ER, ZLexer.ERE);
		myMap.put(ZLexer.SR, ZLexer.SRE);
		return myMap;
	}
	
	private static final Map<Integer, Integer> nofixMap = createNofixMap();

	private static Map<Integer, Integer> createNofixMap() {
		Map<Integer, Integer> myMap = new HashMap<Integer, Integer>(4);
		myMap.put(ZLexer.I, ZLexer.I); // symmetry, not actually used
		myMap.put(ZLexer.L, ZLexer.L);
		myMap.put(ZLexer.ER, ZLexer.ER);
		myMap.put(ZLexer.SR, ZLexer.SR);
		return myMap;
	}

	boolean isRelation = false;
	Map<Integer, Integer> currentMap = functionMap;
	Map<Integer, Integer> currentFixityMap = nofixMap;

	@Override
	public void enterRelationOperatorTemplate(ZOperatorParser.RelationOperatorTemplateContext ctx) {
		isRelation = true;
		currentMap = relationMap;
	}
	
	void replaceType(Token token, int type) {
		((WritableToken)token).setType(currentMap.get(type));
		associations.put(token.getText(), currentMap.get(type));
		
		if(isRightAssoc) {
			rightAssociativity.add(token.getText());
		}
	}
	
	void replaceFixName(ParserRuleContext ctx) {
		try {
			Method name = ctx.getClass().getMethod("NAME");
			Method argName = ctx.getClass().getMethod("argName");
			Method listName = ctx.getClass().getMethod("listName");
			
			int nameType = ZLexer.I;
			ZOperatorParser.ArgNameContext argCtx = (ZOperatorParser.ArgNameContext)argName.invoke(ctx);
			ZOperatorParser.ListNameContext listCtx = (ZOperatorParser.ListNameContext)listName.invoke(ctx);
			
			if (argCtx != null) {
				nameType = ZLexer.L;
				replaceType(argCtx.NAME().getSymbol(), currentFixityMap.get(ZLexer.ER));
			} else if (listCtx != null) {
				nameType = ZLexer.L;
				replaceType(listCtx.NAME().getSymbol(), currentFixityMap.get(ZLexer.SR));
			}
			
			replaceType(((TerminalNode)name.invoke(ctx)).getSymbol(), currentFixityMap.get(nameType));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public void exitOptArgName(ZOperatorParser.OptArgNameContext ctx) {
		replaceType(ctx.NAME().getSymbol(), ZLexer.ES);
	}

	@Override
	public void exitOptListName(ZOperatorParser.OptListNameContext ctx) {
		replaceType(ctx.NAME().getSymbol(), ZLexer.SS);
	}

	@Override
	public void exitPrefixName(ZOperatorParser.PrefixNameContext ctx) {
		currentFixityMap = prefixMap;
		replaceFixName(ctx);
	}

	@Override
	public void exitPostfixName(ZOperatorParser.PostfixNameContext ctx) {
		currentFixityMap = postfixMap;
		replaceFixName(ctx);
	}

	@Override
	public void exitInfixName(ZOperatorParser.InfixNameContext ctx) {
		currentFixityMap = infixMap;
		replaceFixName(ctx);
	}

	@Override
	public void exitNofixName(ZOperatorParser.NofixNameContext ctx) {
		currentFixityMap = nofixMap;
		replaceFixName(ctx);
	}

	@Override
	public void exitRelationOperatorTemplate(ZOperatorParser.RelationOperatorTemplateContext ctx) {
		isRelation = false;
		currentMap = functionMap;
	}
	
	@Override
	public void exitAssoc(ZOperatorParser.AssocContext ctx) {
		isRightAssoc = "rightassoc".equals(ctx.getText());
	}
	
	@Override
	public void exitCategoryTemplate(ZOperatorParser.CategoryTemplateContext ctx) {
		isRightAssoc = false;
	}
}
