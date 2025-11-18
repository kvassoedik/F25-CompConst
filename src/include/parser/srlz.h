#pragma once

#include "parser/fwd_structs.h"
#include <iostream>

#define AST_SRLZTYPE_METHOD \
virtual void serializeType(::ast::srlz::options o) const { ::ast::srlz::type(*this, o); }

namespace ast {
namespace srlz {

struct SerializeOptions {
    std::ostream& os;
    bool ir{false};
};
using options = const SerializeOptions&;

#define AST_SRLZ_OPTIONS_DEFAULT_CONSTR \
::ast::srlz::SerializeOptions{.os = std::cout}

void type(const Type& node, options o = AST_SRLZ_OPTIONS_DEFAULT_CONSTR);
void type(const TypeRef& node, options o = AST_SRLZ_OPTIONS_DEFAULT_CONSTR);
void type(const RoutineType& node, options o = AST_SRLZ_OPTIONS_DEFAULT_CONSTR);
void type(const ArrayType& node, options o = AST_SRLZ_OPTIONS_DEFAULT_CONSTR);
void type(const RecordType& node, options o = AST_SRLZ_OPTIONS_DEFAULT_CONSTR);

#undef AST_SRLZ_OPTIONS_DEFAULT_CONSTR

}
}