import java.awt.Color;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.SequenceInputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.antlr.v4.gui.TreeViewer;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.WritableToken;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

public class Test {
	public static void main(String[] args) {

		Path path1 = Paths.get(
				"standard_toolkit_operator_templates.utf8");
		Path path2 = Paths.get("/Users/ataylor/Sched.utf8");
//		Path path2 = Paths.get("apD.utf8");
//		Path path2 = Paths.get("as.utf8");
//		Path path2 = Paths.get("birthdaybook.utf8");
//		Path path2 = Paths.get("birthdaybook_simple.utf8");
//		Path path2 = Paths.get("birthdaybook_unfolded.utf8");
//		Path path2 = Paths.get("buffer.utf8");
//		Path path2 = Paths.get("ch3.utf8");
//		Path path2 = Paths.get("ch4.utf8");
//		Path path2 = Paths.get("ch5.utf8");
//		Path path2 = Paths.get("ch6.utf8");
//		Path path2 = Paths.get("ch7.utf8");
//		Path path2 = Paths.get("ds.utf8");
//		Path path2 = Paths.get("dstest.utf8");
//		Path path2 = Paths.get("fs.utf8");
//		Path path2 = Paths.get("lcycle1.utf8");
//		Path path2 = Paths.get("lcycle2.utf8");
//		Path path2 = Paths.get("lcycle3.utf8");
//		Path path2 = Paths.get("lcycle4.utf8");
//		Path path2 = Paths.get("ns.utf8");
//		Path path2 = Paths.get("optemp.utf8");
//		Path path2 = Paths.get("posix.utf8");
//		Path path2 = Paths.get("selfParentLatex.utf8");
//		Path path2 = Paths.get("ss.utf8");
//		Path path2 = Paths.get("sstest.utf8");
//		Path path2 = Paths.get("tokeneer_41_2_all_merged_for_parsing.utf8");
//		Path path2 = Paths.get("tokeneer_41_2_all_merged_for_typechecking.utf8");

		FileInputStream f1;
		FileInputStream f2;
		try {
			f1 = new FileInputStream(path1.toString());
			f2 = new FileInputStream(path2.toString());
			SequenceInputStream seq = new SequenceInputStream(f1, f2);

			ANTLRInputStream stream = new ANTLRInputStream(seq);
			ZLexer lexer = new ZLexer(stream);
			CommonTokenStream tokens = new CommonTokenStream(lexer);
			ZOperatorParser parser = new ZOperatorParser(tokens);
			ParserRuleContext tree = parser.specification();

			ParseTreeWalker walker = new ParseTreeWalker();
			ZOperatorListener ol = new ZOperatorListener();
			walker.walk(ol, tree);

			for (Token token : tokens.getTokens()) {
				Integer mapping = ol.associations.get(token.getText());
				if (mapping != null) {
					((WritableToken) token).setType(mapping);
				}
			}

			ZSupport.rightAssociativity = ol.rightAssociativity;
			tokens.reset();
			ZParser parser2 = new ZParser(tokens);
			ParserRuleContext tree2 = parser2.specification();

			// show AST in GUI
			JFrame frame = new JFrame("Antlr AST");
			JPanel panel = new JPanel();
			TreeViewer viewr = new TreeViewer(Arrays.asList(parser2.getRuleNames()), tree2);
			viewr.setScale(1.5);// scale a little
			panel.add(viewr);
			JScrollPane scroll = new JScrollPane(panel);
			frame.add(scroll);
			frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
			frame.setSize(500, 500);
			frame.setVisible(true);
			frame.setBackground(Color.WHITE);
			panel.setBackground(Color.WHITE);
			f1.close();
			f2.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
