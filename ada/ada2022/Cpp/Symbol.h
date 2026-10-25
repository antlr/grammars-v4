#pragma once
#include <string>
#include <set>
#include <map>
#include "TypeClassification.h"

class Symbol {
public:
    std::string name;
    std::set<TypeClassification> classification;
    std::map<std::string, Symbol*> members;
    Symbol* parent = nullptr;
    bool predefined = false;
    bool isComposite = false;
    std::string definedFile;
    int definedLine = 0;
    int definedColumn = 0;

    std::string toString() const;
};
