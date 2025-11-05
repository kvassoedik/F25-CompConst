#pragma once

#include "parser/fwd_structs.h"
#include <memory>
#include <string>

namespace analyzer {

bool isPrimitiveType(const std::shared_ptr<Ast::Type>& t);
bool areTypesEqual(const std::shared_ptr<Ast::Type>& t1, const std::shared_ptr<Ast::Type>& t2);
bool isErrorType(const std::shared_ptr<Ast::Type> type);
std::string stringifyType(const std::shared_ptr<Ast::Type>& t);
inline const char* boolToStr(bool b) { return b ? "true" : "false"; };

}