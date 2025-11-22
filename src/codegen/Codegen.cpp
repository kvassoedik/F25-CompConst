#include "codegen/Codegen.h"
#include "parser/structs.h"
#include "analyzer/utils.h"
#include "llvm/IR/Verifier.h"
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
    primaryOffsets_.reserve(16);

    mainTys_.floatTy = llvm::Type::getFloatTy(*context_);
    // heapObjTypes_.heapObjPtr = StructType::create(*context_, "hoptr");
    // heapObjTypes_.heapObjPtr->setBody({llvm::PointerType::getInt64Ty(*context_)});

    // heapObjTypes_.array = StructType::create(*context_, "heapArray");
    // heapObjTypes_.array->setBody({llvm::PointerType::getInt64Ty(*context_)});

    // heapObjTypes_.record = StructType::create(*context_, "heapRecord");
    // heapObjTypes_.record->setBody({llvm::PointerType::getInt64Ty(*context_)});
}

int Codegen::configure(int* argc, char** argv) {
    return 0;
}

void Codegen::run() {
    initMetaGlobals();
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
            llvm::AllocaInst* llVar = tmpBuilder.CreateAlloca(llInitializer->getType(), nullptr, node.id);
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
    std::string fmtStr;
    fmtStr.resize(std::max(node.args.size()*3, 1UL));

    size_t i = 0;
    for (auto& arg: node.args) {
        fmtStr[i++] = '%';

        char c;
        switch (arg->type->code) {
        case TypeEnum::Bool: { c = 's'; break; }
        case TypeEnum::Int: { c = 'd'; break; }
        case TypeEnum::Real: { c = 'f'; break; }
        default:
            llvm_unreachable("gen PrintStmt: unexpected arg type");
        }

        fmtStr[i++] = c;
        if (i+1 < fmtStr.size())
            fmtStr[i++] = ' ';
    }
    fmtStr[i] = '\n';

    llvm::Constant* llFmtStr = builder_->CreateGlobalStringPtr(fmtStr, ".fmt", 0, nullptr, false);

    std::vector<llvm::Value*> llArgs;
    llArgs.reserve(node.args.size()+1);
    llArgs.push_back(llFmtStr);

    for (auto& arg: node.args) {
        llvm::Value* llArg = arg->codegen(*this);
        if (llArg == nullptr)
            throw std::runtime_error("print arg has returned nullptr from codegen");

        // Converting bool to string on output
        if (arg->type->code == TypeEnum::Bool) {
            llArg = builder_->CreateSelect(llArg, globals_.strTrue, globals_.strFalse, "boolStr");
        }

        llArgs.push_back(llArg);
    }

    llvm::Function *llPrintf = module_->getFunction("printf");
    builder_->CreateCall(llPrintf, llArgs);
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
    case ExprEnum::BoolLiteral: return ConstantInt::get(
        llvm::Type::getInt1Ty(*context_),
        static_cast<const BoolLiteral&>(node).val
    );
    case ExprEnum::IntLiteral: return ConstantInt::get(
        llvm::Type::getInt32Ty(*context_),
        static_cast<const IntLiteral&>(node).val
    );
    case ExprEnum::RealLiteral: return ConstantFP::get(
        mainTys_.floatTy,
        static_cast<const RealLiteral&>(node).val
    );
    }
    llvm_unreachable("Codegen Expr: unexpected code " + std::to_string(node.code));
}

llvm::Value* Codegen::gen(const ast::BinaryExpr& node) {
    llvm::Value* llLeft = node.left->codegen(*this);
    if (llLeft == nullptr)
        throw std::runtime_error("Codegen BinaryExpr left got nullptr");
    llvm::Value* llRight = node.right->codegen(*this);
    if (llRight == nullptr)
        throw std::runtime_error("Codegen BinaryExpr right got nullptr");

    switch (node.code) {
    case ExprEnum::Add: {
        if (node.type->code == TypeEnum::Int) {
            return builder_->CreateAdd(llLeft, llRight, "add");
        } else if (node.type->code == TypeEnum::Real) {
            convertToFloat(llLeft);
            convertToFloat(llRight);
            return builder_->CreateFAdd(llLeft, llRight, "fadd");
        } else
            llvm_unreachable("Codegen BinaryExpr: unexpected type " + std::to_string(node.type));
    }
    case ExprEnum::Subtract: {
        if (node.type->code == TypeEnum::Int) {
            return builder_->CreateSub(llLeft, llRight, "sub");
        } else if (node.type->code == TypeEnum::Real) {
            convertToFloat(llLeft);
            convertToFloat(llRight);
            return builder_->CreateFSub(llLeft, llRight, "fsub");
        } else
            llvm_unreachable("Codegen BinaryExpr: unexpected type " + std::to_string(node.type));
    }
    case ExprEnum::Multiply: {
        if (node.type->code == TypeEnum::Int) {
            llLeft->getType()->dump();
            llRight->getType()->dump();
            return builder_->CreateMul(llLeft, llRight, "mul");
        } else if (node.type->code == TypeEnum::Real) {
            convertToFloat(llLeft);
            convertToFloat(llRight);
            return builder_->CreateFMul(llLeft, llRight, "fmul");
        } else
            llvm_unreachable("Codegen BinaryExpr: unexpected type " + std::to_string(node.type));
    }
    case ExprEnum::Divide: {
        if (node.type->code == TypeEnum::Int) {
            return builder_->CreateSDiv(llLeft, llRight, "div");
        } else if (node.type->code == TypeEnum::Real) {
            convertToFloat(llLeft);
            convertToFloat(llRight);
            return builder_->CreateFDiv(llLeft, llRight, "fdiv");
        } else
            llvm_unreachable("Codegen BinaryExpr: unexpected type " + std::to_string(node.type));
    }
    case ExprEnum::Modulo: {
        if (node.type->code == TypeEnum::Int) {
            return builder_->CreateSRem(llLeft, llRight, "mul");
        } else
            llvm_unreachable("Codegen BinaryExpr: unexpected type " + std::to_string(node.type));
    }
    case ExprEnum::And: return builder_->CreateAnd(llLeft, llRight, "and");
    case ExprEnum::Or: return builder_->CreateOr(llLeft, llRight, "or");
    case ExprEnum::Xor: return builder_->CreateXor(llLeft, llRight, "xor");
    case ExprEnum::LESS_THAN: return builder_->CreateICmpSLT(llLeft, llRight, "lt");
    case ExprEnum::LESS_OR_EQUAL: return builder_->CreateICmpSLE(llLeft, llRight, "le");
    case ExprEnum::MORE_THAN: return builder_->CreateICmpSGT(llLeft, llRight, "gt");
    case ExprEnum::MORE_OR_EQUAL: return builder_->CreateICmpSGE(llLeft, llRight, "ge");
    case ExprEnum::EQUAL: return builder_->CreateICmpEQ(llLeft, llRight, "eq");
    case ExprEnum::UNEQUAL: return builder_->CreateICmpNE(llLeft, llRight, "ne");
    }
    llvm_unreachable("Codegen BinaryExpr: unexpected code " + std::to_string(node.code));
}

llvm::Value* Codegen::gen(const ast::UnaryExpr& node) {
    llvm::Value* llVal = node.val->codegen(*this);
    if (llVal == nullptr)
        throw std::runtime_error("Codegen UnaryExpr: val got nullptr");

    switch (node.code) {
    case ExprEnum::Negate: return builder_->CreateNeg(llVal, "neg");
    case ExprEnum::Not: return builder_->CreateNot(llVal, "not");
    }
    llvm_unreachable("Codegen UnaryExpr: unexpected code " + std::to_string(node.code));
}

llvm::Value* Codegen::gen(const ast::IdRef& node) {
    auto varDecl = node.ref.lock();
    if (!varDecl)
        llvm_unreachable("Codegen IdRef does not refer to any declaration");

    auto it = vars_.find(varDecl.get());
    if (it == vars_.end())
        llvm_unreachable("Codegen IdRef ref is not yet added to vars map");

    llvm::Type* llTy = getType(*varDecl->type); // pointers are opaque, thus we need to get the type from AST again
    llvm::Value* llLoad = builder_->CreateLoad(llTy, it->second);

    if (node.next) {
        primaryType_ = node.type.get();

        // Struct GEP requires a 0 offset first
        primaryOffsets_.push_back(
            llvm::ConstantInt::get(*context_, llvm::APInt(32, 0))
        );

        // primaryOffsets_ will be filled by each .next node
        node.next->codegen(*this);

        llvm::Type* llTy = getType(*varDecl->type);
        llvm::Value* llGep = builder_->CreateGEP(llTy, llLoad, primaryOffsets_);
        primaryOffsets_.clear();
        return llGep;
    }

    return llLoad;
}

llvm::Value* Codegen::gen(const ast::RecordMember& node) {
    if (primaryOffsets_.empty())
        llvm_unreachable("Codegen RecordMember: rogue");
    if (primaryType_->code != TypeEnum::Record)
        llvm_unreachable("Codegen RecordMember: parent type is not a record");

    auto& members = static_cast<RecordType*>(primaryType_)->members;
    size_t offset = 0;
    for (auto& mem: members) {
        if (mem->id == node.id) {
            primaryType_ = mem->type.get();
            break;
        }
        offset++;
    }
    if (offset == members.size())
        llvm_unreachable("Codegen RecordMember: member not found by name");

    primaryOffsets_.push_back(
        llvm::ConstantInt::get(*context_, llvm::APInt(32, offset))
    );

    if (node.next)
        node.next->codegen(*this);

    return nullptr;
}

llvm::Value* Codegen::gen(const ast::ArrayAccess& node) {
    if (primaryOffsets_.empty())
        llvm_unreachable("Codegen ArrayAccess: rogue");
    if (primaryType_->code != TypeEnum::Array)
        llvm_unreachable("Codegen ArrayAccess: parent type is not an array");

    llvm::Value* llOffset = node.val->codegen(*this);
    if (llOffset == nullptr)
        llvm_unreachable("Codegen ArrayAccess: val got null");
    
    primaryOffsets_.push_back(llOffset);

    if (node.next) {
        primaryType_ = node.getType()->elemType.get();
        node.next->codegen(*this);
    }

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
    case TypeEnum::Real: return mainTys_.floatTy;
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

// LLVM builder CreateGlobalStrPtr doesn't work outside of insertion block, hence done manually
llvm::Constant* Codegen::newGlobalStrGlobalScope(const char* str, const char* label) {
    auto *llData = llvm::ConstantDataArray::getString(*context_, str, false /* null terminator */);
    auto *llGlobal = new llvm::GlobalVariable(
        *module_, llData->getType(), true,
        llvm::GlobalValue::PrivateLinkage, llData, label
    );
    llGlobal->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

    auto *llZero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0);
    llvm::Constant *indices[] = {llZero, llZero};

    return llvm::ConstantExpr::getGetElementPtr(llData->getType(), llGlobal, indices);
}

void Codegen::initMetaGlobals() {
    module_->getOrInsertFunction(
        "printf",
        llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(*context_),
            llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(*context_)),
            true /* vararg */
        )
    );

    globals_.strTrue = newGlobalStrGlobalScope("true", ".str_true");
    globals_.strFalse = newGlobalStrGlobalScope("false", ".str_false");
}

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
    case ExprEnum::BoolLiteral: return ConstantInt::get(
        llvm::Type::getInt1Ty(*context_),
        static_cast<const BoolLiteral&>(node).val
    );
    case ExprEnum::IntLiteral: return ConstantInt::get(
        llvm::Type::getInt32Ty(*context_),
        static_cast<const IntLiteral&>(node).val
    );
    case ExprEnum::RealLiteral: return ConstantFP::get(
        mainTys_.floatTy,
        static_cast<const RealLiteral&>(node).val
    );
    default:
        llvm_unreachable("Codegen getConstInitializer: unexpected code");
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

void Codegen::convertToFloat(llvm::Value*& llVal) {
    if (llVal->getType()->isIntegerTy())
        llVal = builder_->CreateSIToFP(llVal, mainTys_.floatTy, "itof");
}

llvm::Value* Codegen::newHeapObject(llvm::TypeSize bitSize) {
    return nullptr;
}

void Codegen::heapObjUseCountInc() {

}

void Codegen::heapObjUseCountDecr() {

}