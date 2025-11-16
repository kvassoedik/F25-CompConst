#include "codegen/Codegen.h"
#include "parser/structs.h"

using codegen::Codegen;
using namespace ast;
using namespace llvm;

Codegen::Codegen(std::shared_ptr<ast::Ast> ast)
    : ast_(std::move(ast))
    , context_(std::make_unique<llvm::LLVMContext>())
    , builder_(std::make_unique<llvm::IRBuilder<>>(*context_))
    , module_(std::make_unique<llvm::Module>("Module", *context_))
{}

int Codegen::configure(int* argc, char** argv) {
    return 0;
}

void Codegen::run() {
    
}

llvm::Value* Codegen::gen(const ast::Expr& node) {
    switch (node.code) {
    case ExprEnum::BoolLiteral: return ConstantInt::getSigned(
        llvm::Type::getInt1Ty(*context_),
        static_cast<const BoolLiteral&>(node).val
    );
    case ExprEnum::IntLiteral: return ConstantInt::getSigned(
        llvm::Type::getInt32Ty(*context_),
        static_cast<const IntLiteral&>(node).val
    );
    case ExprEnum::RealLiteral: return ConstantFP::get(
        llvm::Type::getFloatTy(*context_),
        static_cast<const RealLiteral&>(node).val
    );
    default:
        llvm_unreachable("Codegen Expr got unsupported code");
    }
}

llvm::Value* Codegen::gen(const ast::TypeDecl& node) {
    
}

llvm::Value* Codegen::gen(const ast::Var& node) {

}

llvm::Value* Codegen::gen(const ast::Routine& node) {

}

llvm::Value* Codegen::gen(const ast::Assignment& node) {

}