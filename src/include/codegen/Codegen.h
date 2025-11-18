#pragma once

#include "codegen/visitor.h"
#include "parser/Ast.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include <memory>
#include <unordered_map>
#include <vector>

namespace codegen {
class Codegen final : public Visitor {
public:
    Codegen(std::shared_ptr<ast::Ast> ast);

    int configure(int* argc, char** argv);
    void run();

    llvm::Value* gen(const ast::Var& node) override;
    llvm::Value* gen(const ast::Routine& node) override;
    llvm::Value* gen(const ast::TypeDecl& node) override;
    llvm::Value* gen(const ast::Assignment& node) override;
    llvm::Value* gen(const ast::PrintStmt& node) override;
    llvm::Value* gen(const ast::IfStmt& node) override;
    llvm::Value* gen(const ast::ForStmt& node) override;
    llvm::Value* gen(const ast::WhileStmt& node) override;
    llvm::Value* gen(const ast::ReturnStmt& node) override;
    llvm::Value* gen(const ast::Expr& node) override;
    llvm::Value* gen(const ast::BinaryExpr& node) override;
    llvm::Value* gen(const ast::UnaryExpr& node) override;
    llvm::Value* gen(const ast::IdRef& node) override;
    llvm::Value* gen(const ast::ArrayAccess& node) override;
    llvm::Value* gen(const ast::RoutineCall& node) override;

    llvm::Type* genType(const ast::Type& node) override;
    llvm::Type* genType(const ast::ArrayType& node) override;
    llvm::Type* genType(const ast::RecordType& node) override;
private:
    void genGlobalVars();
    void genRoutines();
    void dump();

    llvm::Type* getType(const ast::Type& node);
    llvm::Constant* getConstInitializer(const ast::Expr& node);
    llvm::FunctionType* genRoutineType(const ast::RoutineType& node);

    llvm::Value* newHeapObject(llvm::TypeSize bitSize);
    void heapObjUseCountInc();
    void heapObjUseCountDecr();
private:
    std::unordered_map<const ast::Var*, llvm::Value*> vars_;
    std::unordered_map<std::string, llvm::Type*> typeHashMap_;
    struct {
        llvm::StructType *heapObjPtr;
        // llvm::StructType *array, *record;
    } heapObjTypes_;

    std::shared_ptr<ast::Ast> ast_;
    std::unique_ptr<llvm::LLVMContext> context_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;
    std::unique_ptr<llvm::Module> module_;

    bool globalScope_{true};
    bool isMainRoutine_{false};
};
}