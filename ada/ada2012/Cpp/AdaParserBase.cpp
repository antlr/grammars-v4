#include "antlr4-runtime.h"
#include "AdaParserBase.h"
#include "AdaLexer.h"
#include "AdaParser.h"
#include <algorithm>
#include <iostream>

AdaParserBase::AdaParserBase(antlr4::TokenStream * input) : antlr4::Parser(input)
{
}

bool AdaParserBase::IsAggregate()
{
    if (noSemantics.count("IsAggregate")) return true;
    auto stream = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    if (!stream) return false;
    auto lt1 = stream->LT(1);
    if (!lt1 || lt1->getType() != AdaLexer::LP) return false;
    int depth = 0;
    for (int i = 2; ; i++) {
        auto t = stream->LT(i);
        if (!t || t->getType() == antlr4::Token::EOF) break;
        if (t->getType() == AdaLexer::LP) {
            depth++;
        } else if (t->getType() == AdaLexer::RP) {
            if (depth == 0) break;
            depth--;
        } else if (depth == 0) {
            if (t->getType() == AdaLexer::COMMA) return true;
            if (t->getType() == AdaLexer::ARROW) return true;
            if (t->getType() == AdaLexer::WITH) return true;
            if (t->getType() == AdaLexer::NULL_) {
                auto next = stream->LT(i + 1);
                if (next && next->getType() == AdaLexer::RECORD) return true;
            }
        }
    }
    if (!_expectedTypeStack.empty()) {
        auto expected = _expectedTypeStack.back();
        if (expected && expected->isComposite) return true;
    }
    return false;
}

bool AdaParserBase::IsTypeName()
{
    if (noSemantics.count("IsTypeName")) return true;
    auto stream = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    if (!stream) return false;
    auto lt1 = stream->LT(1);
    if (!lt1 || lt1->getType() != AdaLexer::IDENTIFIER_) return false;
    auto firstName = lt1->getText();
    auto resolved = _st.resolve(firstName);
    return resolved != nullptr && resolved->classification.count(TypeClassification::TypeName_);
}

void AdaParserBase::EnterDeclaration()
{
    auto context = dynamic_cast<antlr4::ParserRuleContext*>(getContext());
    while (context != nullptr) {
        if (auto ftd = dynamic_cast<AdaParser::Full_type_declarationContext*>(context)) {
            auto defId = ftd->defining_identifier();
            if (defId) {
                bool isComposite = false;
                auto typeDef = ftd->type_definition();
                if (typeDef) {
                    isComposite = typeDef->record_type_definition() != nullptr || typeDef->array_type_definition() != nullptr;
                }
                defineSymbol(defId->getText(), TypeClassification::TypeName_, defId->getStart(), isComposite);
            }
            return;
        }
        if (auto std_ = dynamic_cast<AdaParser::Subtype_declarationContext*>(context)) {
            auto defId = std_->defining_identifier();
            if (defId) {
                bool isComposite = false;
                auto si = std_->subtype_indication();
                if (si) {
                    auto sm = si->subtype_mark();
                    if (sm) {
                        auto baseSym = _st.resolve(sm->getText());
                        if (baseSym) isComposite = baseSym->isComposite;
                    }
                }
                defineSymbol(defId->getText(), TypeClassification::TypeName_, defId->getStart(), isComposite);
            }
            return;
        }
        if (auto od = dynamic_cast<AdaParser::Object_declarationContext*>(context)) {
            auto defIdList = od->defining_identifier_list();
            if (defIdList) {
                for (auto defId : defIdList->defining_identifier()) {
                    defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
                }
            }
            return;
        }
        if (auto nd = dynamic_cast<AdaParser::Number_declarationContext*>(context)) {
            auto defIdList = nd->defining_identifier_list();
            if (defIdList) {
                for (auto defId : defIdList->defining_identifier()) {
                    defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
                }
            }
            return;
        }
        if (auto spd = dynamic_cast<AdaParser::Subprogram_declarationContext*>(context)) {
            defineSubprogramFromSpec(spd->subprogram_specification());
            return;
        }
        if (auto spb = dynamic_cast<AdaParser::Subprogram_bodyContext*>(context)) {
            defineSubprogramFromSpec(spb->subprogram_specification());
            return;
        }
        if (auto pkd = dynamic_cast<AdaParser::Package_declarationContext*>(context)) {
            auto pkgSpec = pkd->package_specification();
            if (pkgSpec) {
                auto dpun = pkgSpec->defining_program_unit_name();
                if (dpun && dpun->defining_identifier())
                    defineSymbol(dpun->defining_identifier()->getText(), TypeClassification::PackageName_, dpun->defining_identifier()->getStart());
            }
            return;
        }
        if (auto pkb = dynamic_cast<AdaParser::Package_bodyContext*>(context)) {
            auto dpun = pkb->defining_program_unit_name();
            if (dpun && dpun->defining_identifier())
                defineSymbol(dpun->defining_identifier()->getText(), TypeClassification::PackageName_, dpun->defining_identifier()->getStart());
            return;
        }
        if (auto exd = dynamic_cast<AdaParser::Exception_declarationContext*>(context)) {
            auto defIdList = exd->defining_identifier_list();
            if (defIdList) {
                for (auto defId : defIdList->defining_identifier()) {
                    defineSymbol(defId->getText(), TypeClassification::ExceptionName_, defId->getStart());
                }
            }
            return;
        }
        if (auto ttd = dynamic_cast<AdaParser::Task_type_declarationContext*>(context)) {
            auto defId = ttd->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::TypeName_, defId->getStart());
            return;
        }
        if (auto cd = dynamic_cast<AdaParser::Component_declarationContext*>(context)) {
            auto defIdList = cd->defining_identifier_list();
            if (defIdList) {
                for (auto defId : defIdList->defining_identifier()) {
                    defineSymbol(defId->getText(), TypeClassification::ComponentName_, defId->getStart());
                }
            }
            return;
        }
        if (auto gi = dynamic_cast<AdaParser::Generic_instantiationContext*>(context)) {
            auto dpuns = gi->defining_program_unit_name();
            if (!dpuns.empty()) {
                auto defId = dpuns[0]->defining_identifier();
                if (defId) {
                    auto tc = TypeClassification::PackageName_;
                    if (gi->PROCEDURE() || gi->FUNCTION()) tc = TypeClassification::SubprogramName_;
                    defineSymbol(defId->getText(), tc, defId->getStart());
                }
            }
            auto dds = gi->defining_designator();
            if (!dds.empty()) {
                auto dpun = dds[0]->defining_program_unit_name();
                if (dpun && dpun->defining_identifier())
                    defineSymbol(dpun->defining_identifier()->getText(), TypeClassification::SubprogramName_, dpun->defining_identifier()->getStart());
            }
            return;
        }
        if (auto ps = dynamic_cast<AdaParser::Parameter_specificationContext*>(context)) {
            auto defIdList = ps->defining_identifier_list();
            if (defIdList) {
                for (auto defId : defIdList->defining_identifier()) {
                    defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
                }
            }
            return;
        }
        if (auto lps = dynamic_cast<AdaParser::Loop_parameter_specificationContext*>(context)) {
            auto defId = lps->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
            return;
        }
        if (auto els = dynamic_cast<AdaParser::Enumeration_literal_specificationContext*>(context)) {
            auto defId = els->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::EnumerationLiteral_, defId->getStart());
            return;
        }
        if (auto its = dynamic_cast<AdaParser::Iterator_specificationContext*>(context)) {
            auto defId = its->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
            return;
        }
        if (auto itd = dynamic_cast<AdaParser::Incomplete_type_declarationContext*>(context)) {
            auto defId = itd->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::TypeName_, defId->getStart());
            return;
        }
        if (auto pvtd = dynamic_cast<AdaParser::Private_type_declarationContext*>(context)) {
            auto defId = pvtd->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::TypeName_, defId->getStart());
            return;
        }
        if (auto ped = dynamic_cast<AdaParser::Private_extension_declarationContext*>(context)) {
            auto defId = ped->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::TypeName_, defId->getStart(), true);
            return;
        }
        if (auto ord = dynamic_cast<AdaParser::Object_renaming_declarationContext*>(context)) {
            auto defId = ord->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
            return;
        }
        if (auto erd = dynamic_cast<AdaParser::Exception_renaming_declarationContext*>(context)) {
            auto defId = erd->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::ExceptionName_, defId->getStart());
            return;
        }
        if (auto prd = dynamic_cast<AdaParser::Package_renaming_declarationContext*>(context)) {
            auto dpun = prd->defining_program_unit_name();
            if (dpun && dpun->defining_identifier())
                defineSymbol(dpun->defining_identifier()->getText(), TypeClassification::PackageName_, dpun->defining_identifier()->getStart());
            return;
        }
        if (auto fctd = dynamic_cast<AdaParser::Formal_complete_type_declarationContext*>(context)) {
            auto defId = fctd->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::TypeName_, defId->getStart());
            return;
        }
        if (auto fitd = dynamic_cast<AdaParser::Formal_incomplete_type_declarationContext*>(context)) {
            auto defId = fitd->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::TypeName_, defId->getStart());
            return;
        }
        if (auto fod = dynamic_cast<AdaParser::Formal_object_declarationContext*>(context)) {
            auto defIdList = fod->defining_identifier_list();
            if (defIdList) {
                for (auto defId : defIdList->defining_identifier()) {
                    defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
                }
            }
            return;
        }
        if (auto fpd = dynamic_cast<AdaParser::Formal_package_declarationContext*>(context)) {
            auto defId = fpd->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::PackageName_, defId->getStart());
            return;
        }
        if (auto cps = dynamic_cast<AdaParser::Choice_parameter_specificationContext*>(context)) {
            auto defId = cps->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
            return;
        }
        if (auto eis = dynamic_cast<AdaParser::Entry_index_specificationContext*>(context)) {
            auto defId = eis->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
            return;
        }
        if (auto erod = dynamic_cast<AdaParser::Extended_return_object_declarationContext*>(context)) {
            auto defId = erod->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
            return;
        }
        if (auto ed = dynamic_cast<AdaParser::Entry_declarationContext*>(context)) {
            auto defId = ed->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::SubprogramName_, defId->getStart());
            return;
        }
        if (auto eb = dynamic_cast<AdaParser::Entry_bodyContext*>(context)) {
            auto defId = eb->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::SubprogramName_, defId->getStart());
            return;
        }
        if (auto staskd = dynamic_cast<AdaParser::Single_task_declarationContext*>(context)) {
            auto defId = staskd->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
            return;
        }
        if (auto ptd = dynamic_cast<AdaParser::Protected_type_declarationContext*>(context)) {
            auto defId = ptd->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::TypeName_, defId->getStart());
            return;
        }
        if (auto sprd = dynamic_cast<AdaParser::Single_protected_declarationContext*>(context)) {
            auto defId = sprd->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
            return;
        }
        if (auto tb = dynamic_cast<AdaParser::Task_bodyContext*>(context)) {
            auto defId = tb->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
            return;
        }
        if (auto pb = dynamic_cast<AdaParser::Protected_bodyContext*>(context)) {
            auto defId = pb->defining_identifier();
            if (defId) defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
            return;
        }
        if (auto ds = dynamic_cast<AdaParser::Discriminant_specificationContext*>(context)) {
            auto defIdList = ds->defining_identifier_list();
            if (defIdList) {
                for (auto defId : defIdList->defining_identifier()) {
                    defineSymbol(defId->getText(), TypeClassification::ObjectName_, defId->getStart());
                }
            }
            return;
        }
        context = dynamic_cast<antlr4::ParserRuleContext*>(context->parent);
    }
}

void AdaParserBase::defineSubprogramFromSpec(antlr4::ParserRuleContext* specCtx) {
    auto spec = dynamic_cast<AdaParser::Subprogram_specificationContext*>(specCtx);
    if (!spec) return;
    auto procSpec = spec->procedure_specification();
    if (procSpec) {
        auto dpun = procSpec->defining_program_unit_name();
        if (dpun && dpun->defining_identifier())
            defineSymbol(dpun->defining_identifier()->getText(), TypeClassification::SubprogramName_, dpun->defining_identifier()->getStart());
        return;
    }
    auto funcSpec = spec->function_specification();
    if (funcSpec) {
        auto dd = funcSpec->defining_designator();
        if (dd) {
            auto dpun = dd->defining_program_unit_name();
            if (dpun && dpun->defining_identifier())
                defineSymbol(dpun->defining_identifier()->getText(), TypeClassification::SubprogramName_, dpun->defining_identifier()->getStart());
        }
    }
}

void AdaParserBase::defineSymbol(const std::string& name, TypeClassification classification, antlr4::Token* token, bool isComposite) {
    auto sym = new Symbol(); // Owned by SymbolTable via parent scope's members
    sym->name = name;
    sym->classification.insert(classification);
    sym->isComposite = isComposite;
    if (token) {
        sym->definedFile = token->getTokenSource() ? token->getTokenSource()->getSourceName() : "";
        sym->definedLine = static_cast<int>(token->getLine());
        sym->definedColumn = static_cast<int>(token->getCharPositionInLine());
    }
    _st.define(sym);
}

void AdaParserBase::EnterScope() { _st.pushBlockScope(); }
void AdaParserBase::ExitScope() { _st.popBlockScope(); }
void AdaParserBase::PushExpectedType() { _expectedTypeStack.push_back(nullptr); }
void AdaParserBase::PopExpectedType() {
    if (!_expectedTypeStack.empty()) _expectedTypeStack.pop_back();
}

void AdaParserBase::OutputSymbolTable() {
    if (outputSymbolTableFlag) {
        std::cerr << _st.toString() << std::endl;
    }
}

void AdaParserBase::ParsePragmas()
{
    auto stream = dynamic_cast<antlr4::BufferedTokenStream *>(getTokenStream());
    if (!stream) return;
    stream->fill();
    auto allTokens = stream->getTokens();
    const int PRAGMA_CHANNEL = 2;
    std::vector<antlr4::Token *> *currentPragma = nullptr;
    std::vector<std::vector<antlr4::Token *>> pragmas;
    for (auto token : allTokens) {
        if (static_cast<int>(token->getChannel()) != PRAGMA_CHANNEL) continue;
        if (token->getType() == AdaLexer::PRAGMA) {
            pragmas.push_back({});
            currentPragma = &pragmas.back();
            currentPragma->push_back(token);
        } else if (currentPragma != nullptr) {
            currentPragma->push_back(token);
            if (token->getType() == AdaLexer::SEMI) {
                currentPragma = nullptr;
            }
        }
    }
    for (auto &pragmaTokens : pragmas) {
        std::vector<std::unique_ptr<antlr4::CommonToken>> ownedTokens;
        std::vector<std::unique_ptr<antlr4::Token>> sourceTokens;
        for (auto t : pragmaTokens) {
            auto ct = std::make_unique<antlr4::CommonToken>(
                t->getType(), t->getText());
            ct->setLine(t->getLine());
            ct->setCharPositionInLine(t->getCharPositionInLine());
            ct->setChannel(antlr4::Token::DEFAULT_CHANNEL);
            ct->setStartIndex(t->getStartIndex());
            ct->setStopIndex(t->getStopIndex());
            ct->setTokenIndex(t->getTokenIndex());
            ownedTokens.push_back(std::move(ct));
        }
        auto eof = std::make_unique<antlr4::CommonToken>(
            antlr4::Token::EOF, "");
        eof->setChannel(antlr4::Token::DEFAULT_CHANNEL);
        ownedTokens.push_back(std::move(eof));
        for (auto &ct : ownedTokens) {
            sourceTokens.push_back(std::unique_ptr<antlr4::Token>(ct.release()));
        }
        auto tokenSource = std::make_unique<antlr4::ListTokenSource>(std::move(sourceTokens));
        auto tokenStream = std::make_unique<antlr4::CommonTokenStream>(tokenSource.get());
        AdaParser parser(tokenStream.get());
        parser.removeErrorListeners();
        parser.addErrorListener(&getErrorListenerDispatch());
        parser.pragmaRule();
    }
}
