#include "CParserBase.h"
#include "CLexer.h"
#include "CParser.h"
#include <algorithm>

CParserBase::CParserBase(antlr4::TokenStream *input)
    : antlr4::Parser(input) {
    _st = SymbolTable();
}

// ---------- Helpers ----------

std::shared_ptr<Symbol> CParserBase::resolveWithOutput(antlr4::Token *token) {
    if (!token) return nullptr;
    std::string text = token->getText();
    auto resolved = _st.resolve(text);
    if (outputAppliedOccurrences_ && resolved) {
        auto appliedLoc = getSourceLocation(token);
        std::cerr << "Applied occurrence: " << text
                  << " at " << appliedLoc.file << ":" << appliedLoc.line << ":" << appliedLoc.column
                  << " -> Defined at " << resolved->getDefinedFile()
                  << ":" << resolved->getDefinedLine()
                  << ":" << resolved->getDefinedColumn() << std::endl;
    }
    return resolved;
}

CParserBase::SourceLocation CParserBase::getSourceLocation(antlr4::Token *token) {
    if (!token) return {"", 0, 0};

    std::string fileName = "<unknown>";
    int line = static_cast<int>(token->getLine());
    int column = static_cast<int>(token->getCharPositionInLine());
    int lineAdjusted = line;

    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    if (!ts) return {fileName, lineAdjusted, column};

    size_t ind = token->getTokenIndex();

    // Search back from token index to find last LineDirective
    for (int j = static_cast<int>(ind); j >= 0; j--) {
        auto *t = ts->get(j);
        if (!t) break;
        if (t->getType() == CLexer::LineDirective) {
            std::string txt = t->getText();
            // Parse "# line_num \"filename\"" or "# line_num filename"
            std::istringstream iss(txt);
            std::string hash;
            int dirLine = 0;
            std::string file;
            iss >> hash;
            // hash might be "#" or "#line"
            if (hash == "#") {
                std::string next;
                iss >> next;
                // Could be a number or "line"
                try {
                    dirLine = std::stoi(next);
                } catch (...) {
                    // It was "line", read the actual number
                    iss >> dirLine;
                }
            }
            // Read the rest as filename
            iss >> file;
            // Remove quotes
            if (file.size() >= 2 && file.front() == '"' && file.back() == '"') {
                file = file.substr(1, file.size() - 2);
            }

            int lineDirective = static_cast<int>(t->getLine());
            int lineDiff = line - lineDirective;
            lineAdjusted = lineDiff + dirLine - 1;
            fileName = file;
            break;
        }
    }

    return {fileName, lineAdjusted, column};
}

antlr4::Token* CParserBase::getDeclarationToken(antlr4::ParserRuleContext *ctx) {
    if (!ctx) return nullptr;

    // We expect a DeclaratorContext or work with it generically
    auto *declaratorCtx = dynamic_cast<CParser::DeclaratorContext*>(ctx);
    if (!declaratorCtx) return nullptr;

    auto *directDeclarator = declaratorCtx->directDeclarator();
    if (directDeclarator) {
        // Recurse into nested declarator
        auto *more = directDeclarator->declarator();
        if (more) {
            auto *token = getDeclarationToken(more);
            if (token) return token;
        }
        if (directDeclarator->Identifier()) {
            return directDeclarator->Identifier()->getSymbol();
        }
    }

    return nullptr;
}

std::string CParserBase::getDeclarationId(antlr4::ParserRuleContext *ctx) {
    auto *token = getDeclarationToken(ctx);
    return token ? token->getText() : "";
}

// ---------- Semantic Predicates ----------

bool CParserBase::IsAlignmentSpecifier(int k) {
    if (noSemantics_.count("IsAlignmentSpecifier")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *lt1 = ts->LT(k);
    if (debug_) std::cerr << "IsAlignmentSpecifier " << lt1->toString() << std::flush;
    auto resolved = resolveWithOutput(lt1);
    bool result = false;
    if (resolved && resolved->getClassification().count(TypeClassification::AlignmentSpecifier_)) {
        result = true;
    }
    if (debug_) std::cerr << " " << result << std::endl;
    return result;
}

bool CParserBase::IsAtomicTypeSpecifier(int k) {
    if (noSemantics_.count("IsAtomicTypeSpecifier")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *lt1 = ts->LT(k);
    if (debug_) std::cerr << "IsAtomicTypeSpecifier " << lt1->toString() << std::flush;
    auto resolved = resolveWithOutput(lt1);
    bool result = false;
    if (resolved && resolved->getClassification().count(TypeClassification::AtomicTypeSpecifier_)) {
        result = true;
    }
    if (debug_) std::cerr << " " << result << std::endl;
    return result;
}

bool CParserBase::IsAttributeDeclaration() {
    if (noSemantics_.count("IsAttributeDeclaration")) return true;
    return IsAttributeSpecifierSequence();
}

bool CParserBase::IsAttributeSpecifier() {
    if (noSemantics_.count("IsAttributeSpecifier")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *lt1 = ts->LT(1);
    if (debug_) std::cerr << "IsAttributeSpecifier " << lt1->toString() << std::flush;
    bool result = lt1->getType() == CLexer::LeftBracket;
    if (debug_) std::cerr << " " << result << std::endl;
    return result;
}

bool CParserBase::IsAttributeSpecifierSequence() {
    if (noSemantics_.count("IsAttributeSpecifierSequence")) return true;
    return IsAttributeSpecifier();
}

bool CParserBase::IsDeclaration() {
    if (noSemantics_.count("IsDeclaration")) return true;
    if (debug_) std::cerr << "IsDeclaration" << std::endl;
    bool result = IsDeclarationSpecifiers()
                  || IsAttributeSpecifierSequence()
                  || IsStaticAssertDeclaration()
                  || IsAttributeDeclaration();
    if (debug_) std::cerr << "IsDeclaration " << result << std::endl;
    return result;
}

bool CParserBase::IsDeclarationSpecifier() {
    if (noSemantics_.count("IsDeclarationSpecifier")) return true;
    if (debug_) std::cerr << "IsDeclarationSpecifier" << std::endl;
    bool result = IsStorageClassSpecifier()
                  || IsTypeSpecifier()
                  || IsTypeQualifier()
                  || (IsFunctionSpecifier() && !IsGnuAttributeBeforeDeclarator())
                  || IsAlignmentSpecifier();
    if (debug_) std::cerr << "IsDeclarationSpecifier " << result << std::endl;
    return result;
}

bool CParserBase::IsTypeSpecifierQualifier(int k) {
    if (noSemantics_.count("IsTypeSpecifierQualifier")) return true;
    bool result = IsTypeSpecifier(k)
                  || IsTypeQualifier(k)
                  || IsAlignmentSpecifier(k);
    return result;
}

bool CParserBase::IsDeclarationSpecifiers() {
    return IsDeclarationSpecifier();
}

bool CParserBase::IsEnumSpecifier(int k) {
    if (noSemantics_.count("IsEnumSpecifier")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *lt1 = ts->LT(k);
    if (debug_) std::cerr << "IsEnumSpecifier " << lt1->toString() << std::flush;
    bool result = lt1->getType() == CLexer::Enum;
    if (debug_) std::cerr << " " << result << std::endl;
    return result;
}

bool CParserBase::IsFunctionSpecifier() {
    if (noSemantics_.count("IsFunctionSpecifier")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *lt1 = ts->LT(1);
    if (debug_) std::cerr << "IsFunctionSpecifier " << lt1->toString() << std::flush;
    auto resolved = resolveWithOutput(lt1);
    bool result = false;
    if (resolved && resolved->getClassification().count(TypeClassification::FunctionSpecifier_)) {
        result = true;
    }
    if (debug_) std::cerr << " " << result << std::endl;
    return result;
}

bool CParserBase::IsGnuAttributeBeforeDeclarator(int k) {
    if (noSemantics_.count("IsGnuAttributeBeforeDeclarator")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    if (!ts) return false;
    int i = k;
    if (ts->LT(i)->getType() != CLexer::Attribute) return false;
    i++;
    int depth = 0;
    while (true) {
        auto *t = ts->LT(i++);
        if (t->getType() == antlr4::Token::EOF) return false;
        if (t->getType() == CLexer::LeftParen) depth++;
        else if (t->getType() == CLexer::RightParen) { depth--; if (depth == 0) break; }
    }
    int next = ts->LT(i)->getType();
    return next == CLexer::Identifier || next == CLexer::Star || next == CLexer::LeftParen;
}

bool CParserBase::IsStatement() {
    if (noSemantics_.count("IsStatement")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *t1 = ts->LT(1);
    auto *t2 = ts->LT(2);
    if (debug_) std::cerr << "IsStatement1 " << t1->toString() << std::endl;
    if (debug_) std::cerr << "IsStatement2 " << t2->toString() << std::endl;
    if (t1->getType() == CLexer::Identifier && t2->getType() == CLexer::Colon) {
        if (debug_) std::cerr << "IsStatement3 true" << std::flush;
        return true;
    }
    bool result = !IsDeclaration();
    if (debug_) std::cerr << "IsStatement " << result << std::flush;
    return result;
}

bool CParserBase::IsStaticAssertDeclaration() {
    if (noSemantics_.count("IsStaticAssertDeclaration")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *token = ts->LT(1);
    if (debug_) std::cerr << "IsStaticAssertDeclaration " << token->toString() << std::flush;
    bool result = token->getType() == CLexer::Static_assert;
    if (debug_) std::cerr << " " << result << std::endl;
    return result;
}

bool CParserBase::IsStorageClassSpecifier() {
    if (noSemantics_.count("IsStorageClassSpecifier")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *lt1 = ts->LT(1);
    if (debug_) std::cerr << "IsStorageClassSpecifier " << lt1->toString() << std::flush;
    auto resolved = resolveWithOutput(lt1);
    bool result = false;
    if (resolved && resolved->getClassification().count(TypeClassification::StorageClassSpecifier_)) {
        result = true;
    }
    if (debug_) std::cerr << " " << result << std::endl;
    return result;
}

bool CParserBase::IsStructOrUnionSpecifier(int k) {
    if (noSemantics_.count("IsStructOrUnionSpecifier")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *token = ts->LT(k);
    if (debug_) std::cerr << "IsStructOrUnionSpecifier " << token->toString() << std::flush;
    bool result = token->getType() == CLexer::Struct ||
                  token->getType() == CLexer::Union;
    if (debug_) std::cerr << " " << result << std::endl;
    return result;
}

bool CParserBase::IsTypedefName(int k) {
    if (noSemantics_.count("IsTypedefName")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *lt1 = ts->LT(k);
    if (debug_) std::cerr << "IsTypedefName " << lt1->toString() << std::flush;
    auto resolved = resolveWithOutput(lt1);
    bool result = false;
    if (!resolved) {
        result = false;
    } else if (resolved->getClassification().count(TypeClassification::Variable_)) {
        result = false;
    } else if (resolved->getClassification().count(TypeClassification::Function_)) {
        result = false;
    } else {
        result = true;
    }
    if (debug_) std::cerr << " " << result << std::endl;
    return result;
}

bool CParserBase::IsTypeofSpecifier(int k) {
    if (noSemantics_.count("IsTypeofSpecifier")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *token = ts->LT(k);
    if (debug_) std::cerr << "IsTypeofSpecifier " << token->toString() << std::flush;
    bool result = token->getType() == CLexer::Typeof ||
                  token->getType() == CLexer::Typeof_unqual;
    if (debug_) std::cerr << " " << result << std::endl;
    return result;
}

bool CParserBase::IsTypeQualifier(int k) {
    if (noSemantics_.count("IsTypeQualifier")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *lt1 = ts->LT(k);
    if (debug_) std::cerr << "IsTypeQualifier " << lt1->toString() << std::flush;
    auto resolved = resolveWithOutput(lt1);
    bool result = false;
    if (resolved && resolved->getClassification().count(TypeClassification::TypeQualifier_)) {
        result = true;
    }
    if (debug_) std::cerr << " " << result << std::endl;
    return result;
}

bool CParserBase::IsTypeSpecifier(int k) {
    if (noSemantics_.count("IsTypeSpecifier")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *lt1 = ts->LT(k);
    if (debug_) std::cerr << "IsTypeSpecifier " << lt1->toString() << std::flush;
    auto resolved = resolveWithOutput(lt1);
    bool result = false;
    if (resolved && resolved->getClassification().count(TypeClassification::TypeSpecifier_)) {
        result = true;
    }

    if (result) {
        if (debug_) std::cerr << " " << result << std::endl;
        return result;
    }

    result = IsAtomicTypeSpecifier(k) || IsStructOrUnionSpecifier(k) || IsEnumSpecifier(k)
             || IsTypedefName(k) || IsTypeofSpecifier(k);
    if (debug_) std::cerr << " " << result << std::endl;
    return result;
}

bool CParserBase::IsInitDeclaratorList() {
    // Cannot be initDeclaratorList if the first thing is a type.
    // Types need to go to preceding declarationSpecifiers.
    if (noSemantics_.count("IsInitDeclaratorList")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *lt1 = ts->LT(1);
    if (debug_) std::cerr << "IsInitDeclaratorList " << lt1->toString() << std::flush;
    auto resolved = resolveWithOutput(lt1);
    bool result = false;
    if (!resolved) {
        result = true;
    } else if (resolved->getClassification().count(TypeClassification::TypeQualifier_) || resolved->getClassification().count(TypeClassification::TypeSpecifier_)) {
        result = false;
    } else {
        result = true;
    }
    if (debug_) std::cerr << " " << result << std::endl;
    return result;
}

bool CParserBase::IsSomethingOfTypeName() {
    if (noSemantics_.count("IsSizeofTypeName")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    if (!ts) return false;
    int lt1Type = ts->LT(1)->getType();
    if (!(lt1Type == CLexer::Sizeof ||
          lt1Type == CLexer::Countof ||
          lt1Type == CLexer::Alignof ||
          lt1Type == CLexer::Maxof ||
          lt1Type == CLexer::Minof)) return false;
    if (ts->LT(2)->getType() != CLexer::LeftParen) return false;
    if (IsTypeName(3)) return true;
    return false;
}

bool CParserBase::IsTypeName(int k) {
    return IsSpecifierQualifierList(k);
}

bool CParserBase::IsSpecifierQualifierList(int k) {
    if (IsGnuAttributeBeforeDeclarator(k)) return true;
    if (IsTypeSpecifierQualifier(k)) return true;
    return false;
}

bool CParserBase::IsCast() {
    if (noSemantics_.count("IsCast")) return true;
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *t1 = ts->LT(1);
    auto *t2 = ts->LT(2);
    if (debug_) std::cerr << "IsCast1 " << t1->toString() << std::endl;
    if (debug_) std::cerr << "IsCast2 " << t2->toString() << std::endl;
    bool result = false;
    if (t1->getType() != CLexer::LeftParen) {
        // not a cast
    } else if (t2->getType() != CLexer::Identifier) {
        // Assume typecast until otherwise.
        result = true;
    } else {
        // Check id.
        auto resolved = resolveWithOutput(t2);
        if (!resolved) {
            result = false;
        } else if (resolved->getClassification().count(TypeClassification::TypeSpecifier_)) {
            result = true;
        }
    }
    if (debug_) std::cerr << "IsCast " << result << std::flush;
    return result;
}

bool CParserBase::IsNullStructDeclarationListExtension() {
    if (noSemantics_.count("IsNullStructDeclarationListExtension")) return true;
    return true;
}

// ---------- Action Methods ----------

void CParserBase::EnterDeclaration() {
    if (debug_) std::cerr << "EnterDeclaration" << std::endl;
    auto *context = getContext();
    while (context != nullptr) {
        // Check if we're in a DeclarationContext
        auto *declarationCtx = dynamic_cast<CParser::DeclarationContext*>(context);
        if (declarationCtx) {
            auto *declSpecs = declarationCtx->declarationSpecifiers();
            std::vector<CParser::DeclarationSpecifierContext*> declSpecList;
            if (declSpecs) {
                declSpecList = declSpecs->declarationSpecifier();
            }

            // Declare any typeSpecifiers that declare something (struct/union names)
            if (!declSpecList.empty()) {
                bool isTypedef = false;
                for (auto *ds : declSpecList) {
                    if (ds->storageClassSpecifier() && ds->storageClassSpecifier()->Typedef()) {
                        isTypedef = true;
                        break;
                    }
                }
                for (auto *ds : declSpecList) {
                    if (ds && ds->typeSpecifier()) {
                        auto *sous = ds->typeSpecifier()->structOrUnionSpecifier();
                        if (sous && sous->Identifier()) {
                            auto *idToken = sous->Identifier()->getSymbol();
                            std::string id = idToken->getText();
                            if (!id.empty()) {
                                if (debug_) std::cerr << "New symbol Declaration1 Declarator " << id << std::endl;
                                auto symbol = std::make_shared<Symbol>();
                                symbol->setName(id);
                                std::unordered_set<TypeClassification> classSet;
                                classSet.insert(TypeClassification::TypeSpecifier_);
                                symbol->setClassification(classSet);
                                auto loc = getSourceLocation(idToken);
                                symbol->setDefinedFile(loc.file);
                                symbol->setDefinedLine(loc.line);
                                symbol->setDefinedColumn(loc.column);
                                _st.define(symbol);
                            }
                        }
                    }
                }
            }

            // Process init-declarator-list
            auto *initDeclList = declarationCtx->initDeclaratorList();
            if (initDeclList) {
                auto initDecls = initDeclList->initDeclarator();

                bool isTypedef = false;
                for (auto *ds : declSpecList) {
                    if (ds->storageClassSpecifier() && ds->storageClassSpecifier()->Typedef()) {
                        isTypedef = true;
                        break;
                    }
                }

                for (auto *initDecl : initDecls) {
                    auto *y = initDecl ? initDecl->declarator() : nullptr;
                    auto *idToken = getDeclarationToken(y);
                    if (idToken) {
                        std::string text = idToken->getText();
                        auto loc = getSourceLocation(idToken);
                        auto symbol = std::make_shared<Symbol>();
                        symbol->setName(text);
                        std::unordered_set<TypeClassification> classSet;
                        if (isTypedef) {
                            classSet.insert(TypeClassification::TypeSpecifier_);
                        } else {
                            classSet.insert(TypeClassification::Variable_);
                        }
                        symbol->setClassification(classSet);
                        symbol->setDefinedFile(loc.file);
                        symbol->setDefinedLine(loc.line);
                        symbol->setDefinedColumn(loc.column);
                        _st.define(symbol);
                        if (debug_) std::cerr << "New symbol Declaration Declarator " << symbol->toString() << std::endl;
                    }
                }
            }
        }

        // Check if we're in a FunctionDefinitionContext
        auto *funcDefCtx = dynamic_cast<CParser::FunctionDefinitionContext*>(context);
        if (funcDefCtx) {
            auto *de = funcDefCtx->declarator();
            auto *dd = de ? de->directDeclarator() : nullptr;
            if (dd && dd->Identifier()) {
                auto *idToken = dd->Identifier()->getSymbol();
                std::string text = idToken->getText();
                auto loc = getSourceLocation(idToken);
                auto symbol = std::make_shared<Symbol>();
                symbol->setName(text);
                std::unordered_set<TypeClassification> classSet;
                classSet.insert(TypeClassification::Function_);
                symbol->setClassification(classSet);
                symbol->setDefinedFile(loc.file);
                symbol->setDefinedLine(loc.line);
                symbol->setDefinedColumn(loc.column);
                _st.define(symbol);
                if (debug_) std::cerr << "New symbol Declarationf Declarator " << symbol->toString() << std::endl;
                return;
            }
        }

        context = dynamic_cast<antlr4::ParserRuleContext*>(context->parent);
    }
}

void CParserBase::EnterScope() {
    if (debug_) std::cerr << "EnterScope" << std::endl;
    _st.pushBlockScope();
}

void CParserBase::ExitScope() {
    if (debug_) std::cerr << "ExitScope" << std::endl;
    _st.popBlockScope();
}

void CParserBase::LookupSymbol() {
    auto *ts = dynamic_cast<antlr4::CommonTokenStream*>(getTokenStream());
    auto *token = ts->LT(-1);
    if (!token) return;
    std::string text = token->getText();
    auto resolved = _st.resolve(text);
    if (outputAppliedOccurrences_ && resolved) {
        auto appliedLoc = getSourceLocation(token);
        std::cerr << "Applied occurrence: " << text
                  << " at " << appliedLoc.file << ":" << appliedLoc.line << ":" << appliedLoc.column
                  << " -> Defined at " << resolved->getDefinedFile()
                  << ":" << resolved->getDefinedLine()
                  << ":" << resolved->getDefinedColumn() << std::endl;
    }
}

void CParserBase::OutputSymbolTable() {
    if (outputSymbolTable_) {
        std::cerr << _st.toString() << std::endl;
    }
}
