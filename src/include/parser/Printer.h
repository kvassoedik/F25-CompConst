#pragma once

#include "parser/fwd_structs.h"
#include <iostream>

#define AST_PRINTTYPE_METHOD \
virtual void printType(::ast::Printer::options o) { ::ast::Printer::printType(*this, o); }

namespace ast {
namespace Printer {

struct PrinterOptions {
    std::ostream& os;
};
using options = const PrinterOptions&;

#define AST_PRINTER_OPTIONS_DEFAULT_CONSTR \
::ast::Printer::PrinterOptions{.os = std::cout}

void printType(Type& node, options o = AST_PRINTER_OPTIONS_DEFAULT_CONSTR);
void printType(TypeRef& node, options o = AST_PRINTER_OPTIONS_DEFAULT_CONSTR);
void printType(RoutineType& node, options o = AST_PRINTER_OPTIONS_DEFAULT_CONSTR);
void printType(ArrayType& node, options o = AST_PRINTER_OPTIONS_DEFAULT_CONSTR);
void printType(RecordType& node, options o = AST_PRINTER_OPTIONS_DEFAULT_CONSTR);

#undef AST_PRINTER_OPTIONS_DEFAULT_CONSTR

}
}