#pragma once

#define AST_PRINT_METHOD \
void print(::Ast::Printer printer&) { printer->print(*this); }