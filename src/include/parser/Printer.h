#pragma once

#include "parser/fwd_structs.h"
#include <iostream>

#define AST_PRINTTYPE_METHOD \
void printType(::Ast::Printer::options o) { ::Ast::Printer::printType(*this, o); }

namespace Ast {
namespace Printer {

struct PrinterOptions {
    std::ostream& os;
};
using options = const PrinterOptions&;

#define AST_PRINTER_OPTIONS_DEFAULT_CONSTR \
::Ast::Printer::PrinterOptions{.os = std::cout}

void printType(Ast::Type& node, options o = AST_PRINTER_OPTIONS_DEFAULT_CONSTR);
void printType(Ast::TypeRef& node, options o = AST_PRINTER_OPTIONS_DEFAULT_CONSTR);
void printType(Ast::RoutineType& node, options o = AST_PRINTER_OPTIONS_DEFAULT_CONSTR);
void printType(Ast::ArrayType& node, options o = AST_PRINTER_OPTIONS_DEFAULT_CONSTR);
void printType(Ast::RecordType& node, options o = AST_PRINTER_OPTIONS_DEFAULT_CONSTR);

#undef AST_PRINTER_OPTIONS_DEFAULT_CONSTR

}
}