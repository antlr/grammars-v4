#include "JavaParser.h"

using namespace antlr4;

bool JavaParserBase::DoLastRecordComponent()
{
    auto ctx = this->getRuleContext();
    auto tctx = dynamic_cast<JavaParser::RecordComponentListContext*>(ctx);
    if (tctx == nullptr) return true;
    auto rcs = tctx->recordComponent();
    if (rcs.empty()) return true;
    int count = rcs.size();
    for (int c = 0; c < count; ++c)
    {
        if (rcs[c]->ELLIPSIS() != nullptr && c + 1 < count)
            return false;
    }
    return true;
}

bool JavaParserBase::IsNotIdentifierAssign()
{
    auto la = this->_input->LA(1);
    switch (la) {
        case JavaParser::IDENTIFIER:
        case JavaParser::MODULE:
        case JavaParser::OPEN:
        case JavaParser::REQUIRES:
        case JavaParser::EXPORTS:
        case JavaParser::OPENS:
        case JavaParser::TO:
        case JavaParser::USES:
        case JavaParser::PROVIDES:
        case JavaParser::WHEN:
        case JavaParser::WITH:
        case JavaParser::TRANSITIVE:
        case JavaParser::YIELD:
        case JavaParser::SEALED:
        case JavaParser::PERMITS:
        case JavaParser::RECORD:
        case JavaParser::VAR:
            break;
        default:
            return true;
    }
    auto la2 = this->_input->LA(2);
    if (la2 != JavaParser::ASSIGN) return true;
    return false;
}
