#pragma once

#include "parser/structs_fwd.h"
#include <memory>
#include <string>

namespace analyzer {

bool isPrimitiveType(const ast::Type& t);
// unfolds TypeRefs
ast::Type& getPureType(ast::Type& t);
const ast::Type& getPureType(const ast::Type& t);
bool areTypesEqual(ast::Type& ty1, ast::Type& ty2);
bool isErrorType(const ast::Type& type);
std::string stringifyType(const ast::Type& t);
inline const char* boolToStr(bool b) { return b ? "true" : "false"; };

}