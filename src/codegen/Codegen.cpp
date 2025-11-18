#include "codegen/Codegen.h"
#include "parser/structs.h"
#include "analyzer/utils.h"
#include <string>
#include <sstream>

using codegen::Codegen;
using namespace ast;
using namespace llvm;

Codegen::Codegen(std::shared_ptr<ast::Ast> ast)
    : ast_(std::move(ast))
    , context_(std::make_unique<llvm::LLVMContext>())
    , builder_(std::make_unique<llvm::IRBuilder<>>(*context_))
    , module_(std::make_unique<llvm::Module>("Module", *context_))
{
    heapObjTypes_.heapObjPtr = StructType::create(*context_, "hoptr");
    heapObjTypes_.heapObjPtr->setBody({llvm::PointerType::getInt64Ty(*context_)});

    // heapObjTypes_.array = StructType::create(*context_, "heapArray");
    // heapObjTypes_.array->setBody({llvm::PointerType::getInt64Ty(*context_)});

    // heapObjTypes_.record = StructType::create(*context_, "heapRecord");
    // heapObjTypes_.record->setBody({llvm::PointerType::getInt64Ty(*context_)});
}

int Codegen::configure(int* argc, char** argv) {
    return 0;
}

void Codegen::run() {
    genGlobalVars();
    genRoutines();
    dump();
}

// ----------------------------------- gen -------------------------------------
// -----------------------------------------------------------------------------

llvm::Value* Codegen::gen(const ast::Var& node) {
    if (globalScope_) {
        llvm::Type* llTy = getType(*node.type);
        module_->getOrInsertGlobal(node.id, llTy);

        GlobalVariable* llGlobalVar = module_->getNamedGlobal(node.id);
        llGlobalVar->setInitializer(getConstInitializer(*node.val));

        vars_.emplace(&node, llGlobalVar);
        return llGlobalVar;
    } else {
        llvm::Function *llParentFn = builder_->GetInsertBlock()->getParent();
        llvm::IRBuilder<> tmpBuilder(
            &llParentFn->getEntryBlock(),
            llParentFn->getEntryBlock().begin()
        );

        if (analyzer::isPrimitiveType(node.type)) {
            llvm::Value* llInitializer = node.val->codegen(*this);
            llvm::AllocaInst* llVar = tmpBuilder.CreateAlloca(llInitializer->getType(), nullptr, llvm::Twine(node.id));
            builder_->CreateStore(llInitializer, llVar);
            vars_.emplace(&node, llVar);
            return llVar;
        } else {
            if (node.type->code != TypeEnum::Array && node.type->code != TypeEnum::Record)
                llvm_unreachable("gen Var got a non-primitive type that is not Array nor Record");

            llvm::Type* llTy = getType(*node.type);
            llvm::Value* llVar = newHeapObject(llTy->getPrimitiveSizeInBits());
            vars_.emplace(&node, llVar);
            return llVar;
        }
    }
}

llvm::Value* Codegen::gen(const ast::Routine& node) {
    if (!node.body)
        llvm_unreachable("gen Routine got a routine with no body");

    isMainRoutine_ = (node.id == "main");

    llvm::FunctionType* llFnTy = genRoutineType(*node.getType());
    Function* llFn = llvm::Function::Create(llFnTy, llvm::Function::ExternalLinkage, node.id, module_.get());

    BasicBlock *entryBasicBlock = BasicBlock::Create(*context_, "entry", llFn);
    builder_->SetInsertPoint(entryBasicBlock);

    for (auto& llParam : llFn->args()) {
        const int paramNo = llParam.getArgNo();
        const auto& param = node.getType()->params[paramNo];

        std::string paramName = param->id;
        llvm::Type *llParamTy = llFn->getFunctionType()->getParamType(paramNo);
        llvm::Value* llVar = builder_->CreateAlloca(llParamTy, nullptr, llvm::Twine(paramName));
        vars_.emplace(param.get(), llVar);
        
        builder_->CreateStore(&llParam, llVar);
    }

    for (auto& u: node.body->units)
         u->codegen(*this);

    if (isMainRoutine_) {
        llvm::APInt llRetValInt(32 /* bitSize */, 0, true /* signed */);
        builder_->CreateRet(llvm::ConstantInt::get(*context_, llRetValInt));
    } else if (llFn->getReturnType()->isVoidTy()) {
        builder_->CreateRetVoid();
    }

    isMainRoutine_ = false;
    return nullptr;
}

llvm::Value* Codegen::gen(const ast::TypeDecl& node) {
    // nothing has to be done, all types have already been converted to their full form during Analyzer stage
    return nullptr;
}

llvm::Value* Codegen::gen(const ast::Assignment& node) {
    if (globalScope_)
        llvm_unreachable("non-elided at compile-time assignment in global scope is illegal");

    return nullptr;
}

llvm::Value* Codegen::gen(const ast::PrintStmt& node) {
    return nullptr;
}

llvm::Value* Codegen::gen(const ast::IfStmt& node) {
    return nullptr;
}

llvm::Value* Codegen::gen(const ast::ForStmt& node) {
    return nullptr;
}

llvm::Value* Codegen::gen(const ast::WhileStmt& node) {
    return nullptr;
}

llvm::Value* Codegen::gen(const ast::ReturnStmt& node) {
    if (isMainRoutine_) {
        llvm::APInt llRetValInt(32, 0, true);
        builder_->CreateRet(llvm::ConstantInt::get(*context_, llRetValInt));
    } else {
        if (node.val) {
            llvm::Value* llRetVal = node.val->codegen(*this);
            builder_->CreateRet(llRetVal);
        } else {
            builder_->CreateRetVoid();
        }
    }
    return nullptr;
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

llvm::Value* Codegen::gen(const ast::BinaryExpr& node) {
    return nullptr;
}

llvm::Value* Codegen::gen(const ast::UnaryExpr& node) {
    return nullptr;
}

llvm::Value* Codegen::gen(const ast::IdRef& node) {
    return nullptr;
}

llvm::Value* Codegen::gen(const ast::ArrayAccess& node) {
    return nullptr;
}

llvm::Value* Codegen::gen(const ast::RoutineCall& node) {
    return nullptr;
}

// --------------------------------- genType -----------------------------------
// -----------------------------------------------------------------------------

llvm::Type* Codegen::genType(const ast::Type& node) {
    switch (node.code) {
    case TypeEnum::Bool: return llvm::Type::getInt1Ty(*context_);
    case TypeEnum::Int: return llvm::Type::getInt32Ty(*context_);
    case TypeEnum::Real: return llvm::Type::getFloatTy(*context_);
    default:
        llvm_unreachable("genType on Type class called with invalid code");
    }
}

llvm::Type* Codegen::genType(const ast::ArrayType& node) {
    llvm::Type* llElemTy = getType(*node.elemType);
    llvm::Constant* llSize = getConstInitializer(*node.size);

    llvm::Type* llTy = StructType::create({
        // ref_count
        llvm::Type::getInt32Ty(*context_),
        // static array itself
        llvm::ArrayType::get(llElemTy, static_cast<llvm::ConstantInt*>(llSize)->getSExtValue())
    });
    return llTy->getPointerTo();
}

llvm::Type* Codegen::genType(const ast::RecordType& node) {
    std::vector<llvm::Type*> members(node.members.size() + 1);
    // ref_count
    members.push_back(llvm::Type::getInt32Ty(*context_));

    for (auto& member: node.members) {
        llvm::Type* llMemberTy = getType(*member->type);
        members.push_back(llMemberTy);
    }

    llvm::Type* llTy = StructType::create(members);
    return llTy->getPointerTo();
}

// Reusing type string hashes to reuse previously created llvm::Type's
llvm::Type* Codegen::getType(const ast::Type& node) {
    std::stringstream ss;
    node.serializeType({.os = ss, .ir = true});

    auto typeIt = typeHashMap_.find(ss.str());
    if (typeIt != typeHashMap_.end()) {
        return typeIt->second;
    }

    llvm::Type* llTy = node.codegenType(*this);
    typeHashMap_.emplace(std::move(ss).str(), llTy);
    return llTy;
}

// ------------------------------ Private methods ------------------------------
// -----------------------------------------------------------------------------

void Codegen::genGlobalVars() {
    globalScope_ = true;
    for (auto& u: ast_->getRoot()->declMap) {
        if (u.second->isRoutine)
            continue;
        u.second->codegen(*this);
    }

    globalScope_ = false;
}

void Codegen::genRoutines() {
    for (auto& u: ast_->getRoot()->units) {
        auto* routine = dynamic_cast<Routine*>(u.get());
        if (routine)
            routine->codegen(*this);
    }
}

void Codegen::dump() {
    module_->print(llvm::outs(), nullptr);
}

llvm::Constant* Codegen::getConstInitializer(const ast::Expr& node) {
    if (!node.knownPrimitive)
        llvm_unreachable("getConstInitializer called on compile-time unknown expr");

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
        llvm_unreachable("getConstInitializer code switch non-exhaustive");
    }
}

llvm::FunctionType* Codegen::genRoutineType(const ast::RoutineType& node) {
    std::vector<llvm::Type*> llParamTy(node.params.size());
    for (auto &param : node.params)
        llParamTy.push_back(
            getType(*param->type)
        );

    llvm::Type *llRetTy = node.retType
        ?  node.retType->codegenType(*this)
        : llvm::Type::getVoidTy(*context_);
    return llvm::FunctionType::get(llRetTy, llParamTy, false /* isVarArgs */);
}

llvm::Value* Codegen::newHeapObject(llvm::TypeSize bitSize) {
    return nullptr;
}

void Codegen::heapObjUseCountInc() {

}

void Codegen::heapObjUseCountDecr() {

}