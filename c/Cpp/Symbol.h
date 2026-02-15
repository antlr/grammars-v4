#pragma once

#include "TypeClassification.h"
#include <string>
#include <unordered_set>
#include <unordered_map>
#include <memory>
#include <sstream>

class Symbol {
public:
    Symbol() = default;

    const std::string& getName() const { return name_; }
    void setName(const std::string& name) { name_ = name; }

    const std::unordered_set<TypeClassification>& getClassification() const { return classification_; }
    void setClassification(const std::unordered_set<TypeClassification>& classification) { classification_ = classification; }

    const std::unordered_map<std::string, std::shared_ptr<Symbol>>& getMembers() const { return members_; }
    std::unordered_map<std::string, std::shared_ptr<Symbol>>& getMembers() { return members_; }

    Symbol* getParent() const { return parent_; }
    void setParent(Symbol* parent) { parent_ = parent; }

    bool isPredefined() const { return predefined_; }
    void setPredefined(bool predefined) { predefined_ = predefined; }

    const std::string& getDefinedFile() const { return definedFile_; }
    void setDefinedFile(const std::string& file) { definedFile_ = file; }

    int getDefinedLine() const { return definedLine_; }
    void setDefinedLine(int line) { definedLine_ = line; }

    int getDefinedColumn() const { return definedColumn_; }
    void setDefinedColumn(int column) { definedColumn_ = column; }

    std::string toString() const {
        std::ostringstream oss;
        oss << name_;
        oss << " (with classification ";
        bool first = true;
        for (auto c : classification_) {
            if (!first) oss << ", ";
            first = false;
            switch (c) {
                case TypeClassification::Global_: oss << "Global_"; break;
                case TypeClassification::Block_: oss << "Block_"; break;
                case TypeClassification::Function_: oss << "Function_"; break;
                case TypeClassification::Variable_: oss << "Variable_"; break;
                case TypeClassification::TypeSpecifier_: oss << "TypeSpecifier_"; break;
                case TypeClassification::StorageClassSpecifier_: oss << "StorageClassSpecifier_"; break;
                case TypeClassification::TypeQualifier_: oss << "TypeQualifier_"; break;
                case TypeClassification::FunctionSpecifier_: oss << "FunctionSpecifier_"; break;
                case TypeClassification::AlignmentSpecifier_: oss << "AlignmentSpecifier_"; break;
                case TypeClassification::AtomicTypeSpecifier_: oss << "AtomicTypeSpecifier_"; break;
                case TypeClassification::EnumSpecifier_: oss << "EnumSpecifier_"; break;
            }
        }
        oss << ")";
        if (!definedFile_.empty()) {
            oss << " defined at " << definedFile_ << ":" << definedLine_ << ":" << definedColumn_;
        }
        return oss.str();
    }

private:
    std::string name_;
    std::unordered_set<TypeClassification> classification_;
    std::unordered_map<std::string, std::shared_ptr<Symbol>> members_;
    Symbol* parent_ = nullptr;
    bool predefined_ = false;
    std::string definedFile_;
    int definedLine_ = 0;
    int definedColumn_ = 0;
};
