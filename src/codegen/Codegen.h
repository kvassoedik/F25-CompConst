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
    llvm::Value* gen(const ast::RecordMember& node) override;
    llvm::Value* gen(const ast::ArrayAccess& node) override;
    llvm::Value* gen(const ast::RoutineCall& node) override;

    llvm::Type* genType(const ast::Type& node) override;
    llvm::Type* genType(const ast::ArrayType& node) override;
    llvm::Type* genType(const ast::RecordType& node) override;
private:
    llvm::Constant* newGlobalStrGlobalScope(const char* str, const char* label);
    void initMetaGlobals();
    void genMetaFunctions();
    void setupMainEntryPoint();
    void genGlobalVars();
    void genRoutines();
    void dump();

    llvm::Type* getType(const ast::Type& node);
    llvm::Constant* getConstInitializer(const ast::Expr& node);
    llvm::FunctionType* genRoutineType(const ast::RoutineType& node);
    void convertToDouble(llvm::Value*& llVal);
    void codegenBlock(const ast::Block& node, bool isFunctionEntry);

    llvm::Value* newHeapObject(const ast::Type& t, llvm::Type* llTy, llvm::IRBuilder<>& builder);
    void heapObjUseCountInc(llvm::Value* llPtr);
    void heapObjUseCountDecr(llvm::Value* llPtr, const ast::Type& t);
    void heapObjDestroy(llvm::Value* llPtr);
    llvm::Value* codegenPrimaryPtr(const ast::Entity& node);
private:
    struct VarMapping {
        llvm::Value* llVarAllocation;
        llvm::Function* llParentFn;
    };
    std::unordered_map<const ast::Decl*, VarMapping> vars_;
    std::unordered_map<std::string, llvm::Type*> typeHashMap_;
    std::unordered_map<llvm::Type*, llvm::ArrayType*> typeArrayHashMap_;
    struct {
        llvm::Constant *strTrue, *strFalse;
        llvm::Function *main, *refc_inc, *refc_dcr, *arr_bnds_ck;
    } globals_;
    struct {
        llvm::Type *real, *integer, *refcSize, *arraySize;
        llvm::StructType *heapObj, *heapArrayObj;
    } globalTys_;

    std::shared_ptr<ast::Ast> ast_;
    std::unique_ptr<llvm::LLVMContext> context_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;
    std::unique_ptr<llvm::Module> module_;
    std::unique_ptr<llvm::IRBuilder<>> mainEntryBuilder_; // used for inserting global var heap obj initialization in main

    llvm::Value* llPrimaryPtr_{nullptr};
    ast::Type* primaryType_;
    llvm::Type* llPrimaryTy_;

    // if inside a block at least 1 heap object is created, a closing branch is generated and put at the end of the block
    struct HeapObj {
        llvm::Value* llPtr;
        const ast::Type& type;
    };
    struct BlockInfo {
        std::list<HeapObj> heapObjs;
        bool isFunction;
    };
    std::vector<BlockInfo> blockStack_;
    std::vector<HeapObj> tmpHeapObjects_;

    bool globalScope_{true};
    bool isMainRoutine_{false};
    bool getVarPtr_{false};
    struct {
        bool printHeapManagement{false};
    } config_;
};
}