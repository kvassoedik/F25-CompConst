#include "codegen/Codegen.h"
#include "parser/structs.h"
#include "analyzer/utils.h"
#include "llvm/IR/Verifier.h"
#include <string>
#include <sstream>

using codegen::Codegen;
using namespace ast;
using namespace llvm;

#undef llvm_unreachable
#define llvm_unreachable(_p) throw std::runtime_error(_p)

Codegen::Codegen(std::shared_ptr<ast::Ast> ast)
    : ast_(std::move(ast))
    , context_(std::make_unique<llvm::LLVMContext>())
    , builder_(std::make_unique<llvm::IRBuilder<>>(*context_))
    , module_(std::make_unique<llvm::Module>("Module", *context_))
{
    blockStack_.reserve(32);
    tmpHeapObjects_.reserve(16);
    globalTys_.integer = llvm::Type::getInt64Ty(*context_);
    globalTys_.real = llvm::Type::getDoubleTy(*context_);
    globalTys_.heapObj = llvm::StructType::create(*context_, {
        llvm::Type::getInt32Ty(*context_) /* ref_count */
    }, "heap_obj");
}

int Codegen::configure(int* argc, char** argv) {
    return 0;
}

void Codegen::run() {
    initMetaGlobals();
    genMetaFunctions();
    setupMainEntryPoint();
    genGlobalVars();
    genRoutines();
    dump();
}

// ----------------------------------- gen -------------------------------------
// -----------------------------------------------------------------------------

llvm::Value* Codegen::gen(const ast::Var& node) {
    if (globalScope_) {
        llvm::Type* llTy = getType(*node.type);
        GlobalVariable* llGlobalVar;

        if (analyzer::isPrimitiveType(*node.type)) {
            module_->getOrInsertGlobal(node.id, llTy);
            llGlobalVar = module_->getNamedGlobal(node.id);

            llvm::Constant* llInitializer = getConstInitializer(*node.val);
            llGlobalVar->setInitializer(llInitializer);
        } else {
            if (node.type->code != TypeEnum::Array && node.type->code != TypeEnum::Record)
                llvm_unreachable("gen Var got a non-primitive type that is not Array nor Record");

            // if type is complex, store a ptr to its heap obj
            module_->getOrInsertGlobal(node.id, llTy->getPointerTo());
            llGlobalVar = module_->getNamedGlobal(node.id);
            llGlobalVar->setInitializer(
                llvm::ConstantPointerNull::get(llTy->getPointerTo())
            );

            llvm::Value* llPtr = newHeapObject(*node.type, llTy, *mainEntryBuilder_);
            mainEntryBuilder_->CreateStore(llPtr, llGlobalVar);
        }

        vars_.emplace(&node, VarMapping{llGlobalVar, nullptr});
        return llGlobalVar;
    } else {
        llvm::Function *llParentFn = builder_->GetInsertBlock()->getParent();
        llvm::IRBuilder<> tmpBuilder(
            &llParentFn->getEntryBlock(),
            llParentFn->getEntryBlock().begin()
        );

        llvm::AllocaInst* llVar;
        if (analyzer::isPrimitiveType(*node.type)) {
            llvm::Value* llInitializer = node.val->codegen(*this);
            llVar = tmpBuilder.CreateAlloca(llInitializer->getType(), nullptr, node.id);
            builder_->CreateStore(llInitializer, llVar);
        } else {
            if (node.type->code != TypeEnum::Array && node.type->code != TypeEnum::Record)
                llvm_unreachable("gen Var got a non-primitive type that is not Array nor Record");

            llvm::Type* llTy = getType(*node.type);
            llVar = tmpBuilder.CreateAlloca(llTy->getPointerTo(), nullptr, node.id);
            llvm::Value* llInitialzer = newHeapObject(*node.type, llTy, *builder_);
            builder_->CreateStore(llInitialzer, llVar);
        }
        vars_.emplace(&node, VarMapping{llVar, llParentFn});
        return llVar;
    }
}

llvm::Value* Codegen::gen(const ast::Routine& node) {
    if (!node.body)
        llvm_unreachable("gen Routine got a routine with no body");

    isMainRoutine_ = (node.id == "main");
    std::cerr << "routine\n";
    Function* llFn;
    if (isMainRoutine_) {
        llFn = globals_.main;

        BasicBlock* llStartBlk = BasicBlock::Create(*context_, "start", llFn);
        mainEntryBuilder_->CreateBr(llStartBlk);
        // Add new instructions after the mainEntryBuilder_ position
        builder_->SetInsertPoint(llStartBlk);
    } else {
        std::cerr << "gentype\n";
        llvm::FunctionType* llFnTy = genRoutineType(*node.getType());
        llFn = llvm::Function::Create(llFnTy, llvm::Function::ExternalLinkage, node.id, module_.get());

        BasicBlock* llEntryBlk = BasicBlock::Create(*context_, "entry", llFn);
        builder_->SetInsertPoint(llEntryBlk);
    }
    std::cerr << "parameteres\n";
    for (auto& llParam : llFn->args()) {
        const int paramNo = llParam.getArgNo();
        const auto& param = node.getType()->params[paramNo];

        llvm::Type *llParamTy = llFn->getFunctionType()->getParamType(paramNo);
        if (!analyzer::isPrimitiveType(*param->type))
            llParamTy = llParamTy->getPointerTo();
        std::cerr << "param\n";
        llvm::Value* llVar = builder_->CreateAlloca(llParamTy, nullptr, param->id);
        vars_.emplace(param.get(), VarMapping{llVar, llFn});

        builder_->CreateStore(&llParam, llVar);
    }

    codegenBlock(*node.body, true);

    if (isMainRoutine_) {
        llvm::APInt llRetValInt(32 /* bitSize */, 0, true /* signed */);
        builder_->CreateRet(llvm::ConstantInt::get(*context_, llRetValInt));
    } else if (llFn->getReturnType()->isVoidTy()) {
        builder_->CreateRetVoid();
    }
    isMainRoutine_ = false;

    llvm::verifyFunction(*llFn);
    return nullptr;
}

llvm::Value* Codegen::gen(const ast::TypeDecl& node) {
    // nothing has to be done, all types have already been converted to their full form during Analyzer stage
    return nullptr;
}

llvm::Value* Codegen::gen(const ast::Assignment& node) {
    if (globalScope_)
        llvm_unreachable("non-elided at compile-time assignment in global scope is illegal");

    llvm::Value* llLhs = codegenPrimaryPtr(*node.left);
    llvm::Value* llRhs = node.val->codegen(*this);

    if (node.left->type->code == TypeEnum::Int) {
        if (node.val->type->code == TypeEnum::Real) {
            llRhs = builder_->CreateFPToSI(llRhs, globalTys_.integer, "ftoi");
        } else if (node.val->type->code == TypeEnum::Bool) {
            llRhs = builder_->CreateZExt(llRhs, globalTys_.integer, "btoi");
        }
    } else if (node.left->type->code == TypeEnum::Real) {
        if (node.val->type->code == TypeEnum::Int) {
            convertToDouble(llRhs);
        } else if (node.val->type->code == TypeEnum::Bool) {
            llRhs = builder_->CreateZExt(llRhs, globalTys_.integer, "btoi");
            convertToDouble(llRhs);
        }
    } else if (node.left->type->code == TypeEnum::Bool) {
        if (node.val->type->code == TypeEnum::Int) {
            llRhs = builder_->CreateICmpNE(llRhs, llvm::ConstantInt::get(*context_, llvm::APInt(1, 0)), "itob");
        }
    } else if (node.left->type->code == TypeEnum::Array || node.left->type->code == TypeEnum::Record) {
        heapObjUseCountDecr(llLhs, *node.left->type);
        heapObjUseCountInc(llRhs);
    } else
        llvm_unreachable("gen Assignment: unexpected lhs type code");

    builder_->CreateStore(llRhs, llLhs);

    return nullptr;
}

llvm::Value* Codegen::gen(const ast::PrintStmt& node) {
    std::string fmtStr;
    fmtStr.resize(std::max(node.args.size()*3  /* 3 chars for each param, including space */, 1UL));

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

    llvm::Constant* llFmtStr = builder_->CreateGlobalStringPtr(fmtStr, ".fmt");

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
    llvm::Value* llCond = node.condition->codegen(*this);
    if (!llCond)
        llvm_unreachable("gen IfStmt: cond is null");

    llvm::Function* llParentFn = builder_->GetInsertBlock()->getParent();
    llvm::BasicBlock *llThenBlk = llvm::BasicBlock::Create(*context_, "then", llParentFn);
    llvm::BasicBlock *llElseBlk = node.elseBody
        ? llvm::BasicBlock::Create(*context_, "else", llParentFn)
        : nullptr;
    llvm::BasicBlock *llContBlk = llvm::BasicBlock::Create(*context_, "ifcont", llParentFn);

    builder_->CreateCondBr(llCond, llThenBlk, llElseBlk ? llElseBlk : llContBlk);

    builder_->SetInsertPoint(llThenBlk);
    codegenBlock(*node.body, false);
    llvm::Instruction& last = builder_->GetInsertBlock()->back();
    if (!llvm::isa<llvm::ReturnInst>(&last)) {
        builder_->CreateBr(llContBlk);
    }

    if (llElseBlk) {
        builder_->SetInsertPoint(llElseBlk);
        codegenBlock(*node.elseBody, false);
        llvm::Instruction& last = builder_->GetInsertBlock()->back();
        if (!llvm::isa<llvm::ReturnInst>(&last)) {
            builder_->CreateBr(llContBlk);
        }
    }

    builder_->SetInsertPoint(llContBlk);

    return nullptr;
}

llvm::Value* Codegen::gen(const ast::ForStmt& node) {
    std::cerr << "for stmt\n";
    llvm::Function* llParentFn = builder_->GetInsertBlock()->getParent();
    llvm::BasicBlock *llLoopInitBlk = llvm::BasicBlock::Create(*context_, "loopinit", llParentFn);
    llvm::BasicBlock *llLoopBlk = llvm::BasicBlock::Create(*context_, "loop", llParentFn);
    llvm::BasicBlock *llLoopBodyBlk = llvm::BasicBlock::Create(*context_, "loopbody", llParentFn);
    llvm::BasicBlock *llBrkBlk = llvm::BasicBlock::Create(*context_, "loopbrk", llParentFn);

    llvm::Value *llStart, *llEnd;
    auto* intRange = dynamic_cast<IntRange*>(node.range.get());
    if (intRange) {
        llStart = intRange->start->codegen(*this);
        llEnd = intRange->end->codegen(*this);
    } else {
        auto* arrayIdRange = dynamic_cast<ArrayIdRange*>(node.range.get());
        if (!arrayIdRange)
            llvm_unreachable("gen ForStmt: unexpected range");
        llStart = llvm::ConstantInt::get(globalTys_.integer, 0);
        llEnd = nullptr; // TODO
    }

    llvm::Value* llCond;
    if (node.reverse) {
        llCond = builder_->CreateICmpSGE(llEnd, llStart, "reverse_for_ck");
    } else {
        llCond = builder_->CreateICmpSLE(llStart, llEnd, "for_ck");
    }
    builder_->CreateCondBr(llCond, llLoopInitBlk, llBrkBlk);

    builder_->SetInsertPoint(llLoopInitBlk);
    std::cerr << "create alloca\n";
    llvm::Value* llVar = builder_->CreateAlloca(globalTys_.integer, nullptr, node.counter->id);
    vars_.emplace(node.counter.get(), VarMapping{llVar, llParentFn});

    if (node.reverse)
        builder_->CreateStore(llEnd, llVar);
    else
        builder_->CreateStore(llStart, llVar);
    std::cerr << "init done\n";
    builder_->CreateBr(llLoopBodyBlk);

    builder_->SetInsertPoint(llLoopBlk);
    llvm::Value* llCntVal = builder_->CreateLoad(globalTys_.integer, llVar, "for_cnt");
    if (node.reverse) {
        llvm::Value* llStep = builder_->CreateSub(llCntVal, llvm::ConstantInt::get(globalTys_.integer, 1));
        builder_->CreateStore(llStep, llVar, "for_step");
        llCond = builder_->CreateICmpSGE(llStep, llStart, "reverse_for_ck");
    } else {
        llvm::Value* llStep = builder_->CreateAdd(llCntVal, llvm::ConstantInt::get(globalTys_.integer, 1));
        builder_->CreateStore(llStep, llVar, "for_step");
        llCond = builder_->CreateICmpSLE(llStep, llEnd, "for_ck");
    }
    builder_->CreateCondBr(llCond, llLoopBodyBlk, llBrkBlk);

    builder_->SetInsertPoint(llLoopBodyBlk);
    codegenBlock(*node.body, false);
    llvm::Instruction& last = builder_->GetInsertBlock()->back();
    if (!llvm::isa<llvm::ReturnInst>(&last)) {
        builder_->CreateBr(llLoopBlk);
    }

    builder_->SetInsertPoint(llBrkBlk);

    return nullptr;
}

llvm::Value* Codegen::gen(const ast::WhileStmt& node) {
    llvm::Function* llParentFn = builder_->GetInsertBlock()->getParent();
    llvm::BasicBlock *llLoopBlk = llvm::BasicBlock::Create(*context_, "loop", llParentFn);
    llvm::BasicBlock *llLoopBodyBlk = llvm::BasicBlock::Create(*context_, "loopbody", llParentFn);
    llvm::BasicBlock *llBrkBlk = llvm::BasicBlock::Create(*context_, "loopbrk", llParentFn);

    builder_->CreateBr(llLoopBlk);
    builder_->SetInsertPoint(llLoopBlk);

    llvm::Value* llCond = node.condition->codegen(*this);
    if (!llCond)
        llvm_unreachable("gen WhileStmt: cond is null");
    builder_->CreateCondBr(llCond, llLoopBodyBlk, llBrkBlk);

    builder_->SetInsertPoint(llLoopBodyBlk);
    codegenBlock(*node.body, false);
    llvm::Instruction& last = builder_->GetInsertBlock()->back();
    if (!llvm::isa<llvm::ReturnInst>(&last)) {
        builder_->CreateBr(llLoopBlk);
    }

    builder_->SetInsertPoint(llBrkBlk);

    return nullptr;
}

llvm::Value* Codegen::gen(const ast::ReturnStmt& node) {
    if (isMainRoutine_) {
        llvm::APInt llRetValInt(32, 0, true);
        builder_->CreateRet(llvm::ConstantInt::get(*context_, llRetValInt));
    } else {
        llvm::Value* llRetVal;
        bool isRetValPrimitiveType;

        if (node.val) {
            isRetValPrimitiveType = analyzer::isPrimitiveType(*node.val->type);
            if (isRetValPrimitiveType) {
                llRetVal = node.val->codegen(*this);
            } else {
                // Get ptr to heap object, not the object itself
                llRetVal = codegenPrimaryPtr(*node.val);
            }
        }

        bool hasHeapObjs = false;
        for (auto it = blockStack_.rbegin(); it != blockStack_.rend(); ++it) {
            if (!it->heapObjs.empty()
                // edge-case: if it's the value we return, we don't decrement its refc, and thus we don't need to create the branch
                && (it != blockStack_.rbegin() || it->heapObjs.size() > 1 || it->heapObjs.back().llPtr != llRetVal)
            ) {
                hasHeapObjs = true;
                break;
            }
            if (it->isFunction)
                break;
        }
        if (hasHeapObjs) {
            // Calling refc_dcr on all objects in the current scope of the function
            llvm::Function* llParentFn = builder_->GetInsertBlock()->getParent();
            BasicBlock* llClosingBlk = BasicBlock::Create(*context_, "ret_unfold", llParentFn);
            builder_->CreateBr(llClosingBlk);
            builder_->SetInsertPoint(llClosingBlk);

            // Call decrement on all heap objects created in this block AND ones above AS LONG AS the blocks belong to this function
            for (auto it = blockStack_.rbegin(); it != blockStack_.rend(); ++it) {
                for (auto& heapObj: it->heapObjs) {
                    // Don't decrement the refc of the object being returned
                    if (heapObj.llPtr != llRetVal) {
                        heapObjUseCountDecr(heapObj.llPtr, heapObj.type);
                    }
                }
                if (it->isFunction)
                    break;
            }

            BasicBlock* llBackBlk = BasicBlock::Create(*context_, "back", llParentFn);
            builder_->CreateBr(llBackBlk);
            builder_->SetInsertPoint(llBackBlk);
        }
        blockStack_.pop_back();

        if (node.val) {
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
        globalTys_.integer,
        static_cast<const IntLiteral&>(node).val
    );
    case ExprEnum::RealLiteral: return ConstantFP::get(
        globalTys_.real,
        static_cast<const RealLiteral&>(node).val
    );
    }
    llvm_unreachable("Codegen Expr: unexpected code " + std::to_string(static_cast<int>(node.code)));
}

llvm::Value* Codegen::gen(const ast::BinaryExpr& node) {
    llvm::Value* llLeft = node.left->codegen(*this);
    if (llLeft == nullptr)
        throw std::runtime_error("Codegen BinaryExpr left is nullptr");
    llvm::Value* llRight = node.right->codegen(*this);
    if (llRight == nullptr)
        throw std::runtime_error("Codegen BinaryExpr right is nullptr");

    switch (node.code) {
    case ExprEnum::Add: {
        if (node.type->code == TypeEnum::Int) {
            return builder_->CreateAdd(llLeft, llRight, "add");
        } else if (node.type->code == TypeEnum::Real) {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFAdd(llLeft, llRight, "fadd");
        } else
            break;
    }
    case ExprEnum::Subtract: {
        if (node.type->code == TypeEnum::Int) {
            return builder_->CreateSub(llLeft, llRight, "sub");
        } else if (node.type->code == TypeEnum::Real) {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFSub(llLeft, llRight, "fsub");
        } else
            break;
    }
    case ExprEnum::Multiply: {
        if (node.type->code == TypeEnum::Int) {
            return builder_->CreateMul(llLeft, llRight, "mul");
        } else if (node.type->code == TypeEnum::Real) {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFMul(llLeft, llRight, "fmul");
        } else
            break;
    }
    case ExprEnum::Divide: {
        if (node.type->code == TypeEnum::Int) {
            return builder_->CreateSDiv(llLeft, llRight, "div");
        } else if (node.type->code == TypeEnum::Real) {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFDiv(llLeft, llRight, "fdiv");
        } else
            break;
    }
    case ExprEnum::Modulo: {
        if (node.type->code == TypeEnum::Int) {
            return builder_->CreateSRem(llLeft, llRight, "rem");
        } else
            break;
    }
    case ExprEnum::And: return builder_->CreateAnd(llLeft, llRight, "and");
    case ExprEnum::Or: return builder_->CreateOr(llLeft, llRight, "or");
    case ExprEnum::Xor: return builder_->CreateXor(llLeft, llRight, "xor");
    case ExprEnum::LESS_THAN: {
        if (node.left->type->code == TypeEnum::Real || node.right->type->code == TypeEnum::Real) {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFCmpOLT(llLeft, llRight, "flt");
        }
        return builder_->CreateICmpSLT(llLeft, llRight, "lt");
    }
    case ExprEnum::LESS_OR_EQUAL: {
        if (node.left->type->code == TypeEnum::Real || node.right->type->code == TypeEnum::Real) {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFCmpOLE(llLeft, llRight, "fle");
        }
        return builder_->CreateICmpSLE(llLeft, llRight, "le");
    }
    case ExprEnum::MORE_THAN: {
        if (node.left->type->code == TypeEnum::Real || node.right->type->code == TypeEnum::Real) {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFCmpOGT(llLeft, llRight, "fgt");
        }
        return builder_->CreateICmpSGT(llLeft, llRight, "gt");
    }
    case ExprEnum::MORE_OR_EQUAL: {
        if (node.left->type->code == TypeEnum::Real || node.right->type->code == TypeEnum::Real) {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFCmpOGE(llLeft, llRight, "fge");
        }
        return builder_->CreateICmpSGE(llLeft, llRight, "ge");
    }
    case ExprEnum::EQUAL: {
        if (node.left->type->code == TypeEnum::Real || node.right->type->code == TypeEnum::Real) {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFCmpOEQ(llLeft, llRight, "feq");
        }
        return builder_->CreateICmpEQ(llLeft, llRight, "eq");
    }
    case ExprEnum::UNEQUAL: {
        if (node.left->type->code == TypeEnum::Real || node.right->type->code == TypeEnum::Real) {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFCmpONE(llLeft, llRight, "fne");
        }
        return builder_->CreateICmpNE(llLeft, llRight, "ne");
    }
    }
    llvm_unreachable("Codegen BinaryExpr: unexpected code " + std::to_string(static_cast<int>(node.code)));
}

llvm::Value* Codegen::gen(const ast::UnaryExpr& node) {
    llvm::Value* llVal = node.val->codegen(*this);
    if (llVal == nullptr)
        throw std::runtime_error("Codegen UnaryExpr: val is nullptr");

    switch (node.code) {
    case ExprEnum::Negate: return builder_->CreateNeg(llVal, "neg");
    case ExprEnum::Not: return builder_->CreateNot(llVal, "not");
    }
    llvm_unreachable("Codegen UnaryExpr: unexpected code " + std::to_string(static_cast<int>(node.code)));
}

llvm::Value* Codegen::gen(const ast::IdRef& node) {
    auto varDecl = node.ref.lock();
    if (!varDecl)
        llvm_unreachable("Codegen IdRef does not refer to any declaration");

    auto it = vars_.find(varDecl.get());
    if (it == vars_.end())
        llvm_unreachable("Codegen IdRef ref is not yet added to vars map");

    llvm::Value* llVar;
    if (!it->second.llParentFn) {
        // is a global var, load first
        llVar = builder_->CreateLoad(
            llvm::PointerType::getUnqual(*context_),
            it->second.llVarAllocation,
            node.id + "_dref"
        );
    } else {
        llVar = it->second.llVarAllocation;
    }

    llvm::Type* llTy = getType(*varDecl->type); // pointers are opaque, so we need to get the type from AST again
    if (node.next) {
        primaryType_ = varDecl->type.get();
        llPrimaryTy_ = llTy;

        llvm::Value* llDerefVar = builder_->CreateLoad(llPrimaryTy_->getPointerTo(), llVar);
        llPrimaryPtr_ = llDerefVar;
        llvm::Value* llDescendantPtr = node.next->codegen(*this);
        llPrimaryPtr_ = nullptr;
        if (getVarPtr_) {
            // Get ptr
            return llDescendantPtr;
        }

        // Get value
        std::cerr << static_cast<int>(primaryType_->code) << "\n";
        llvm::Type* llDescendantTy = getType(*primaryType_);
        if (!analyzer::isPrimitiveType(*primaryType_))
            llDescendantTy = llDescendantTy->getPointerTo();
        llvm::Value* llLoad = builder_->CreateLoad(llDescendantTy, llDescendantPtr);
        return llLoad;
    }

    if (getVarPtr_)
        return llVar;

    if (!analyzer::isPrimitiveType(*node.type))
        llTy = llTy->getPointerTo();
    llvm::Value* llLoad = builder_->CreateLoad(llTy, llVar);
    return llLoad;
}

llvm::Value* Codegen::gen(const ast::RecordMember& node) {
    if (llPrimaryPtr_ == nullptr)
        llvm_unreachable("Codegen RecordMember: rogue");
    if (primaryType_->code != TypeEnum::Record) {
        if (primaryType_->code == TypeEnum::Array && node.id == "size") {
            // TODO
        }
        llvm_unreachable("Codegen RecordMember: parent type is not a record");
    }

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

    // Return ptr to this field
    llvm::Value* llFieldPtr = builder_->CreateStructGEP(llPrimaryTy_, llPrimaryPtr_,
        offset + 1 /* +1 to skip over ref_count */);

    if (node.next) {
        llPrimaryPtr_ = llFieldPtr;
        llPrimaryTy_ = getType(*primaryType_);
        return node.next->codegen(*this);
    }
    return llFieldPtr;
}

llvm::Value* Codegen::gen(const ast::ArrayAccess& node) {
    if (llPrimaryPtr_ == nullptr)
        llvm_unreachable("gen ArrayAccess: rogue");
    if (primaryType_->code != TypeEnum::Array)
        llvm_unreachable("gen ArrayAccess: parent type is not an array");

    // temporarily setting getVarPtr_ to false so that array index expr retuns us a value, not a ptr
    bool prevGetVarPtr = getVarPtr_;
    getVarPtr_ = false;

    llvm::Value* llOffset = node.val->codegen(*this);
    if (llOffset == nullptr)
        llvm_unreachable("gen ArrayAccess: val is null");
    if (!llOffset->getType()->isIntegerTy(64))
        llvm_unreachable("gen ArrayAccess: expr type is not i64");

    getVarPtr_ = prevGetVarPtr;

    // truncate to i32
    llOffset = builder_->CreateTruncOrBitCast(llOffset, llvm::Type::getInt32Ty(*context_));

    primaryType_ = static_cast<ast::ArrayType*>(primaryType_)->elemType.get();
    // llvm::Value* llArrayPtr = builder_->CreateStructGEP(llPrimaryTy_, llPrimaryPtr_, 1);
    llvm::Value* llArrayPtr = builder_->CreateGEP(
        llPrimaryTy_,
        llPrimaryPtr_,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0),
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 1)}
    );

    auto it = typeArrayHashMap_.find(llPrimaryTy_);
    if (it == typeArrayHashMap_.end())
        llvm_unreachable("gen ArrayAccess: no array type inside hash map");

    // Return ptr to this element
    llvm::Value* llElemPtr = builder_->CreateGEP(it->second, llArrayPtr,
        {
            llvm::ConstantInt::get(*context_, llvm::APInt(32, 0)), 
            llOffset,
        }
    );

    if (node.next) {
        llPrimaryTy_ = getType(*primaryType_);
        llPrimaryPtr_ = llElemPtr;
        return node.next->codegen(*this);
    }
    return llElemPtr;
}

llvm::Value* Codegen::gen(const ast::RoutineCall& node) {
    std::vector<llvm::Value*> llArgs;
    llArgs.reserve(node.args.size());

    for (auto& arg: node.args) {
        llvm::Value* llArg = arg->codegen(*this);
        // std::cerr << "ARG:\n";
        // llArg->dump();
        // llArg->getType()->dump();
        llArgs.push_back(llArg);
    }

    llvm::Function* llFn = module_->getFunction(node.routineId);
    if (!llFn)
        llvm_unreachable("gen RoutineCall: no function found");

    std::cerr << "CALLING\n";
    llvm::Value* llCall = builder_->CreateCall(llFn, llArgs);
    std::cerr << "Call happened\n";
    // If the expr is not set to a variable and it returns a heap object, defer its deletion to the end of the unit in Block
    if (!node.isNamed) {
        auto routineDecl = node.ref.lock();
        if (routineDecl == nullptr)
            llvm_unreachable("RoutineCall ref is null");

        const auto& retType = routineDecl->getType()->retType;
        if (retType && !analyzer::isPrimitiveType(*retType)) {
            std::cerr << "emp;\n";
            tmpHeapObjects_.emplace_back(llCall, *routineDecl->getType()->retType);
        }
        std::cerr << "nexte\n";
    }
    std::cerr << "after tmps\n";
    if (node.next) {
        auto routineDecl = node.ref.lock();
        if (routineDecl == nullptr)
            llvm_unreachable("RoutineCall ref is null");

        primaryType_ = routineDecl->getType()->retType.get();
        llPrimaryTy_ = getType(*primaryType_);

        llPrimaryPtr_ = llCall;
        llvm::Value* llDescendantPtr = node.next->codegen(*this);
        llPrimaryPtr_ = nullptr;

        if (getVarPtr_) {
            // Get ptr
            return llDescendantPtr;
        }

        // Get value
        llvm::Type* llMemberTy = getType(*primaryType_);
        llvm::Value* llLoad = builder_->CreateLoad(llMemberTy, llDescendantPtr);
        return llLoad;
    }
    std::cerr << "Call return\n";
    return llCall;
}

// --------------------------------- genType -----------------------------------
// -----------------------------------------------------------------------------

llvm::Type* Codegen::genType(const ast::Type& node) {
    switch (node.code) {
    case TypeEnum::Bool: return llvm::Type::getInt1Ty(*context_);
    case TypeEnum::Int: return globalTys_.integer;
    case TypeEnum::Real: return globalTys_.real;
    default:
        llvm_unreachable("genType on Type class called with invalid code");
    }
}

llvm::Type* Codegen::genType(const ast::ArrayType& node) {
    llvm::Type* llElemTy = getType(*node.elemType);
    if (llElemTy->isStructTy() || llElemTy->isArrayTy()) {
        // If element type is complex, array will store pointers to heap-allocated objects
        llElemTy = llElemTy->getPointerTo();
    }

    int64_t size;
    if (node.size) {
        llvm::Constant* llSize = getConstInitializer(*node.size);
        size = static_cast<llvm::ConstantInt*>(llSize)->getSExtValue();
    } else {
        size = 0;
    }
    std::cerr << "arr type\n";
    llvm::ArrayType* llArrayTy = llvm::ArrayType::get(llElemTy, size);
    llvm::Type* llTy = StructType::create({
        // ref_count
        llvm::Type::getInt32Ty(*context_),
        // static array itself
        llArrayTy
    });
    std::cerr << "emplace\n";
    typeArrayHashMap_.emplace(llTy, llArrayTy);
    return llTy;
}

llvm::Type* Codegen::genType(const ast::RecordType& node) {
    std::vector<llvm::Type*> members;
    members.reserve(node.members.size() + 1);
    // ref_count
    members.push_back(llvm::Type::getInt32Ty(*context_));

    for (auto& member: node.members) {
        llvm::Type* llMemberTy = getType(*member->type);
        if (llMemberTy->isStructTy() || llMemberTy->isArrayTy()) {
            // If type is complex, the parent struct will store a ptr to heap-allocated object
            members.push_back(llMemberTy->getPointerTo());
        } else {
            members.push_back(llMemberTy);
        }
    }

    llvm::StructType* llTy = StructType::create(members);
    return llTy;
}

// Reusing type string hashes to reuse previously created llvm::Type's
llvm::Type* Codegen::getType(const ast::Type& node) {
    std::stringstream ss;
    std::cerr << "serialze\n";
    node.serializeType({.os = ss, .ir = true});
    std::cerr << "got\n";
    auto typeIt = typeHashMap_.find(ss.str());
    if (typeIt != typeHashMap_.end()) {
        return typeIt->second;
    }
    std::cerr<<"codegenType\n";
    llvm::Type* llTy = node.codegenType(*this);
    typeHashMap_.emplace(std::move(ss).str(), llTy);
    std::cerr<<"done" << llTy << "\n";
    std::cerr << "returning type\n";
    return llTy;
}

// ------------------------------ Private methods ------------------------------
// -----------------------------------------------------------------------------

// LLVM builder CreateGlobalStringPtr doesn't work outside of insertion block, hence done manually
llvm::Constant* Codegen::newGlobalStrGlobalScope(const char* str, const char* label) {
    auto *llData = llvm::ConstantDataArray::getString(*context_, str, true /* null terminator */);
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

    module_->getOrInsertFunction(
        "malloc",
        llvm::FunctionType::get(
            llvm::Type::getInt8Ty(*context_)->getPointerTo(),
            llvm::IntegerType::getInt64Ty(*context_),
            false
        )
    );

    module_->getOrInsertFunction(
        "free",
        llvm::FunctionType::get(
            llvm::Type::getVoidTy(*context_),
            llvm::Type::getInt8Ty(*context_)->getPointerTo(),
            false
        )
    );

    globals_.strTrue = newGlobalStrGlobalScope("true", ".str_true");
    globals_.strFalse = newGlobalStrGlobalScope("false", ".str_false");
}

void Codegen::setupMainEntryPoint() {
    mainEntryBuilder_ = std::make_unique<llvm::IRBuilder<>>(*context_);

    llvm::FunctionType *llTy = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(*context_),
        false /* isVarArgs */
    );
    llvm::Function *llFn = llvm::Function::Create(llTy, llvm::GlobalValue::ExternalLinkage, "main", module_.get());
    llvm::BasicBlock *mainBasicBlock = llvm::BasicBlock::Create(*context_, "entry", llFn);
    mainEntryBuilder_->SetInsertPoint(mainBasicBlock);

    globals_.main = llFn;
    blockStack_.push_back({{}, true}); // empty BlockInfo for the main block
    // global heap objects will be allocated inside main, and thus deallocated at the end of main
}

void Codegen::genMetaFunctions() {
    /* .refc_inc */
    {
        llvm::FunctionType* llFnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*context_),
            {globalTys_.heapObj->getPointerTo()},
            false /* isVarArgs */
        );

        llvm::Function* llFn = llvm::Function::Create(llFnTy, llvm::Function::ExternalLinkage, ".refc_inc", module_.get());
        globals_.refc_inc = llFn;

        BasicBlock* llEntryBlk = BasicBlock::Create(*context_, "entry", llFn);
        builder_->SetInsertPoint(llEntryBlk);

        // Implementation
        llvm::Argument* llPtr = llFn->getArg(0);
        llvm::Value* llRefcntPtr = builder_->CreateStructGEP(
            globalTys_.heapObj,
            llPtr, 0
        );

        llvm::Value* llRefcntVal = builder_->CreateLoad(llvm::Type::getInt32Ty(*context_), llRefcntPtr, "refcnt");
        llvm::Value* llNewRefcnt = builder_->CreateAdd(llRefcntVal, llvm::ConstantInt::get(*context_, llvm::APInt(32, 1)));
        builder_->CreateStore(llNewRefcnt, llRefcntPtr);

        builder_->CreateRetVoid();
    }
    /* .refc_dcr */
    {
        llvm::FunctionType* llFnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*context_),
            {globalTys_.heapObj->getPointerTo()},
            false /* isVarArgs */
        );

        llvm::Function* llFn = llvm::Function::Create(llFnTy, llvm::Function::ExternalLinkage, ".refc_dcr", module_.get());
        globals_.refc_dcr = llFn;

        BasicBlock* llEntryBlk = BasicBlock::Create(*context_, "entry", llFn);
        builder_->SetInsertPoint(llEntryBlk);

        // Implementation
        // llvm::Type *llParamTy = llFnTy->getParamType(0);
        // llvm::Value* llVar = builder_->CreateAlloca(llParamTy, nullptr, "objptr");
        llvm::Argument* llPtr = llFn->getArg(0);
        // builder_->CreateStore(llParam, llVar);

        llvm::Value* llRefcntPtr = builder_->CreateStructGEP(
            globalTys_.heapObj,
            llPtr, 0
        );

        llvm::Function* llParentFn = builder_->GetInsertBlock()->getParent();
        llvm::BasicBlock* llFreeBlk = llvm::BasicBlock::Create(*context_, "free", llParentFn);
        llvm::BasicBlock* llDecrBlk = llvm::BasicBlock::Create(*context_, "dcr", llParentFn);
        llvm::BasicBlock* llContBlk = llvm::BasicBlock::Create(*context_, "continue", llParentFn);

        llvm::Value* llRefcntVal = builder_->CreateLoad(llvm::Type::getInt32Ty(*context_), llRefcntPtr, "refcnt");
        llvm::Value* llCmp = builder_->CreateICmpSLE(llRefcntVal, llvm::ConstantInt::get(*context_, llvm::APInt(32, 1)));
        builder_->CreateCondBr(llCmp, llFreeBlk, llDecrBlk);

        // free
        builder_->SetInsertPoint(llFreeBlk);
        heapObjDestroy(llPtr);
        builder_->CreateBr(llContBlk);

        // dcr
        builder_->SetInsertPoint(llDecrBlk);
        auto* llNewRefcnt = builder_->CreateSub(llRefcntVal, llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 1));
        builder_->CreateStore(llNewRefcnt, llRefcntPtr);
        builder_->CreateBr(llContBlk);

        // continue
        builder_->SetInsertPoint(llContBlk);
        builder_->CreateRetVoid();
    }
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
        globalTys_.integer,
        static_cast<const IntLiteral&>(node).val
    );
    case ExprEnum::RealLiteral: return ConstantFP::get(
        globalTys_.real,
        static_cast<const RealLiteral&>(node).val
    );
    }
    llvm_unreachable("Codegen getConstInitializer: unexpected code");
}

llvm::FunctionType* Codegen::genRoutineType(const ast::RoutineType& node) {
    std::vector<llvm::Type*> llParamTy;
    llParamTy.reserve(node.params.size());

    for (auto &param : node.params) {
        std::cerr << "get param ty\n";
        llvm::Type* llTy = getType(*param->type);
        std::cerr << "GOT?\n";
        if (!analyzer::isPrimitiveType(*param->type))
            llTy = llTy->getPointerTo();
        std::cerr << "param type\n";
        llParamTy.push_back(llTy);
    }
    std::cerr << "gen ret yt\n";
    llvm::Type *llRetTy;
    if (node.retType) {
        llRetTy = node.retType->codegenType(*this);
        if (!analyzer::isPrimitiveType(*node.retType)) {
            llRetTy = llRetTy->getPointerTo();
        }
    } else {
        llRetTy = llvm::Type::getVoidTy(*context_);
    }
    std::cerr << "gened routine type\n";
    return llvm::FunctionType::get(llRetTy, llParamTy, false /* isVarArgs */);
}

void Codegen::convertToDouble(llvm::Value*& llVal) {
    if (llVal->getType()->isIntegerTy())
        llVal = builder_->CreateSIToFP(llVal, globalTys_.real, "itof");
}

void Codegen::codegenBlock(const ast::Block& node, bool isFunctionEntry) {
    llvm::Function *llParentFn = builder_->GetInsertBlock()->getParent();
    BlockInfo* blockInfo;
    
    if (!(isFunctionEntry && isMainRoutine_)) {
        // Add to the block stack for accounting creation of heap objects
        blockStack_.push_back({{}, isFunctionEntry});
    }
    blockInfo = &blockStack_.back();
    std::cerr << "new Block\n";
    for (auto& u: node.units) {
        std::cerr << "unit\n";
        u->codegen(*this);
        // Destroying temporarily-allocated heap objects after a statement has ended
        for (auto& heapObj: tmpHeapObjects_) {
            heapObjUseCountDecr(heapObj.llPtr, heapObj.type);

            // rm from heapObjs, since the count is already decremented right after the statement
            auto it = std::find_if(blockInfo->heapObjs.begin(), blockInfo->heapObjs.end(),
                [&heapObj](HeapObj& obj) { return obj.llPtr == heapObj.llPtr; }
            );
            if (it != blockInfo->heapObjs.end())
                blockInfo->heapObjs.erase(it);
        }
        tmpHeapObjects_.clear();
    }

    if (!blockInfo->heapObjs.empty()) {
        llvm::Instruction& last = builder_->GetInsertBlock()->back();
        if (!llvm::isa<llvm::ReturnInst>(&last)) {
            BasicBlock* llClosingBlk = BasicBlock::Create(*context_, "close", llParentFn);
            builder_->CreateBr(llClosingBlk);
            builder_->SetInsertPoint(llClosingBlk);

            // Call decrement on all heap objects created in this block
            for (auto& heapObj: blockInfo->heapObjs) {
                heapObjUseCountDecr(heapObj.llPtr, heapObj.type);
            }

            BasicBlock* llBackBlk = BasicBlock::Create(*context_, "back", llParentFn);
            builder_->CreateBr(llBackBlk);
            builder_->SetInsertPoint(llBackBlk);
        }
    }

    blockStack_.pop_back();
}

llvm::Value* Codegen::newHeapObject(const ast::Type& type, llvm::Type* llTy, llvm::IRBuilder<>& builder) {
    if (!llTy->isStructTy())
        llvm_unreachable("newHeapObject: unexpected llTy");

    llvm::StructType* llStructTy = static_cast<llvm::StructType*>(llTy);

    const llvm::DataLayout& llDl = module_->getDataLayout();
    uint64_t sizeInBytes = llDl.getTypeAllocSize(llTy);

    llvm::Function *llMalloc = module_->getFunction("malloc");
    llvm::Value* llSize = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), sizeInBytes);
    llvm::Value* llPtr = builder.CreateCall(llMalloc, {llSize}, "obj");
    
    // Initialize heap-object fields
    const auto elemNum = llStructTy->getNumElements();
    std::vector<llvm::Constant*> llInitFields;
    llInitFields.reserve(elemNum);

    // ref_count init to 1
    llInitFields.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 1));
    // Initialize the rest
    for (unsigned i = 1; i < elemNum; ++i) {
        llvm::Type* llElemTy = llStructTy->getElementType(i);
        llInitFields.push_back(llvm::Constant::getNullValue(llElemTy));
    }

    llvm::Value* nullInit = llvm::ConstantStruct::get(llStructTy, llInitFields);
    builder.CreateStore(nullInit, llPtr);

    // For complex types, recursively heap-allocate and initialize the parent's field with the ptr
    if (type.code == TypeEnum::Record) {
        for (unsigned i = 1; i < elemNum; ++i) {
            const ast::Type& elemType = *static_cast<const RecordType&>(type).members[i-1]->type;
            if (!analyzer::isPrimitiveType(elemType)) {
                llvm::Type* llElemTy = getType(elemType);
                llvm::Value* llFieldPtr = newHeapObject(elemType, llElemTy, builder);
                llvm::Value* llGep = builder.CreateStructGEP(llStructTy, llPtr, i, "field_obj");
                builder.CreateStore(llFieldPtr, llGep);
            }
        }
    } else if (type.code == TypeEnum::Array) {
        const ast::Type& elemType = *static_cast<const ast::ArrayType&>(type).elemType;
        if (!analyzer::isPrimitiveType(elemType)) {
            // Allocate all elements immediately
            // TODO
            // llvm::Type* llElemTy = getType(elemType);
            // llvm::Value* llFieldPtr = newHeapObject(elemType, llElemTy, builder);
            // llvm::Value* llGep = builder.CreateStructGEP(llStructTy, llPtr, i, "elem_obj");
            // builder.CreateStore(llFieldPtr, llGep);
        }
    } else
        llvm_unreachable("newHeapObject: unexpected type");

    if (blockStack_.empty())
        llvm_unreachable("newHeapObject called in global scope");
    auto& blockInfo = blockStack_.back();
    blockInfo.heapObjs.emplace_back(llPtr, type);

    std::cerr << "NEW OBJ " << llPtr << "\n";

    return llPtr;
}

void Codegen::heapObjUseCountInc(llvm::Value* llPtr) {
    builder_->CreateCall(globals_.refc_inc, {llPtr});
}

void Codegen::heapObjUseCountDecr(llvm::Value* llPtr, const ast::Type& type) {
    if (type.code == TypeEnum::Record) {
        const auto& members = static_cast<const RecordType&>(type).members;
        for (size_t i = 0; i < members.size(); ++i) {
            if (!analyzer::isPrimitiveType(*members[i]->type)) {
                llvm::Value* llMemberPtr = builder_->CreateStructGEP(globalTys_.heapObj, llPtr, i + 1, "field_obj");
                builder_->CreateCall(globals_.refc_dcr, {llMemberPtr});
            }
        }
    } else if (type.code == TypeEnum::Array) {
        const ast::Type& elemType = *static_cast<const ast::ArrayType&>(type).elemType;
        if (!analyzer::isPrimitiveType(elemType)) {
            // TODO
            // Decrement on each element
        }
    } else
        llvm_unreachable("heapObjUseCountDecr: unexpected provided type");

    builder_->CreateCall(globals_.refc_dcr, {llPtr});
}

void Codegen::heapObjDestroy(llvm::Value* llPtr) {
    llvm::Function* llFreeFn = module_->getFunction("free");
    builder_->CreateCall(llFreeFn, {llPtr});
}

llvm::Value* Codegen::codegenPrimaryPtr(const ast::Entity& node) {
    getVarPtr_ = true;
    llvm::Value* llPtr = node.codegen(*this);
    getVarPtr_ = false;

    if (!llPtr->getType()->isPointerTy())
        llvm_unreachable("codegenIdRefPtr: codegen did not return a Value of ptr type");

    return llPtr;
}