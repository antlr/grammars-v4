#include "Symbol.h"
#include <sstream>

std::string Symbol::toString() const {
    std::string result = name + " (with classification ";
    bool first = true;
    for (auto tc : classification) {
        if (!first) result += ", ";
        result += std::to_string(static_cast<int>(tc));
        first = false;
    }
    result += ")";
    if (isComposite) result += " [composite]";
    if (!definedFile.empty()) {
        result += " defined at " + definedFile + ":" + std::to_string(definedLine) + ":" + std::to_string(definedColumn);
    }
    return result;
}
