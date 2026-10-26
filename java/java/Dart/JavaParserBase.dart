import 'package:antlr4/antlr4.dart';
import 'JavaParser.dart';

abstract class JavaParserBase extends Parser {
    JavaParserBase(TokenStream input) : super(input);

    bool DoLastRecordComponent() {
        final ctx = this.context;
        if (ctx is! RecordComponentListContext) return true;
        final rcs = ctx.children
                ?.whereType<RecordComponentContext>()
                .toList() ?? [];
        if (rcs.isEmpty) return true;
        final count = rcs.length;
        for (int c = 0; c < count; ++c) {
            if (rcs[c].ELLIPSIS() != null && c + 1 < count) return false;
        }
        return true;
    }

    bool IsNotIdentifierAssign() {
        final identifierLikeTokens = {
            JavaParser.TOKEN_IDENTIFIER,
            JavaParser.TOKEN_MODULE,
            JavaParser.TOKEN_OPEN,
            JavaParser.TOKEN_REQUIRES,
            JavaParser.TOKEN_EXPORTS,
            JavaParser.TOKEN_OPENS,
            JavaParser.TOKEN_TO,
            JavaParser.TOKEN_USES,
            JavaParser.TOKEN_PROVIDES,
            JavaParser.TOKEN_WHEN,
            JavaParser.TOKEN_WITH,
            JavaParser.TOKEN_TRANSITIVE,
            JavaParser.TOKEN_YIELD,
            JavaParser.TOKEN_SEALED,
            JavaParser.TOKEN_PERMITS,
            JavaParser.TOKEN_RECORD,
            JavaParser.TOKEN_VAR,
        };
        final la = tokenStream.LA(1);
        if (!identifierLikeTokens.contains(la)) return true;
        final la2 = tokenStream.LA(2);
        return la2 != JavaParser.TOKEN_ASSIGN;
    }
}
