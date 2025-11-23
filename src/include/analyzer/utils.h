#pragma once

#include "parser/fwd_structs.h"
#include <memory>
#include <string>

namespace analyzer {

bool isPrimitiveType(const ast::Type& t);
bool areTypesEqual(const std::shared_ptr<ast::Type>& t1, const std::shared_ptr<ast::Type>& t2);
bool isErrorType(const std::shared_ptr<ast::Type> type);
std::string stringifyType(const std::shared_ptr<ast::Type>& t);
inline const char* boolToStr(bool b) { return b ? "true" : "false"; };

}