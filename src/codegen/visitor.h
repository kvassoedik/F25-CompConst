#pragma once

#include "parser/structs_fwd.h"

namespace llvm {
class Value;
class Type;
}

namespace codegen {
class Visitor {
public:
    ~Visitor() = default;

    virtual llvm::Value* gen(const ast::Var& node) = 0;
    virtual llvm::Value* gen(const ast::Routine& node) = 0;
    virtual llvm::Value* gen(const ast::TypeDecl& node) = 0;
    virtual llvm::Value* gen(const ast::Assignment& node) = 0;
    virtual llvm::Value* gen(const ast::PrintStmt& node) = 0;
    virtual llvm::Value* gen(const ast::IfStmt& node) = 0;
    virtual llvm::Value* gen(const ast::ForStmt& node) = 0;
    virtual llvm::Value* gen(const ast::WhileStmt& node) = 0;
    virtual llvm::Value* gen(const ast::ReturnStmt& node) = 0;
    virtual llvm::Value* gen(const ast::Expr& node) = 0;
    virtual llvm::Value* gen(const ast::BinaryExpr& node) = 0;
    virtual llvm::Value* gen(const ast::UnaryExpr& node) = 0;
    virtual llvm::Value* gen(const ast::IdRef& node) = 0;
    virtual llvm::Value* gen(const ast::RecordMember& node) = 0;
    virtual llvm::Value* gen(const ast::ArrayAccess& node) = 0;
    virtual llvm::Value* gen(const ast::RoutineCall& node) = 0;

    virtual llvm::Type* genType(const ast::Type& node) = 0;
    virtual llvm::Type* genType(const ast::ArrayType& node) = 0;
    virtual llvm::Type* genType(const ast::RecordType& node) = 0;
};
}

#define CODEGEN_METHOD ::llvm::Value* codegen(::codegen::Visitor& visitor) const { return visitor.gen(*this); }
#define CODEGEN_METHOD_SIGNATURE ::llvm::Value* codegen(::codegen::Visitor& visitor) const
#define CODEGEN_TYPE_METHOD ::llvm::Type* codegenType(::codegen::Visitor& visitor) const { return visitor.genType(*this); }