#pragma once

#include "parser/fwd_structs.h"

namespace llvm {
class Value;
}

namespace codegen {
class Visitor {
public:
    ~Visitor() = default;

    virtual llvm::Value* gen(const ast::Expr& node) = 0;
    virtual llvm::Value* gen(const ast::TypeDecl& node) = 0;
    virtual llvm::Value* gen(const ast::Var& node) = 0;
    virtual llvm::Value* gen(const ast::Routine& node) = 0;
    virtual llvm::Value* gen(const ast::Assignment& node) = 0;
};
}

#define CODEGEN_METHOD ::llvm::Value* codegen(::codegen::Visitor& visitor) { return visitor.gen(*this); }
#define CODEGEN_METHOD_SIGNATURE ::llvm::Value* codegen(::codegen::Visitor& visitor)