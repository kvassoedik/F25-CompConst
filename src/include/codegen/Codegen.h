#pragma once

#include "codegen/visitor.h"
#include "parser/Ast.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include <memory>

namespace codegen {
class Codegen final : public Visitor {
public:
    Codegen(std::shared_ptr<ast::Ast> ast);

    int configure(int* argc, char** argv);
    void run();

    llvm::Value* gen(const ast::Expr& node) override;
    llvm::Value* gen(const ast::TypeDecl& node) override;
    llvm::Value* gen(const ast::Var& node) override;
    llvm::Value* gen(const ast::Routine& node) override;
    llvm::Value* gen(const ast::Assignment& node) override;
private:
    std::shared_ptr<ast::Ast> ast_;
    std::unique_ptr<llvm::LLVMContext> context_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;
    std::unique_ptr<llvm::Module> module_;
};
}