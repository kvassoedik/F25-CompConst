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
    : ast_(std::move(ast)), context_(std::make_unique<llvm::LLVMContext>()), builder_(std::make_unique<llvm::IRBuilder<>>(*context_)), module_(std::make_unique<llvm::Module>("Module", *context_))
{
    blockStack_.reserve(32);
    tmpHeapObjects_.reserve(16);
    globalTys_.integer = llvm::Type::getInt64Ty(*context_);
    globalTys_.real = llvm::Type::getDoubleTy(*context_);
    globalTys_.refcSize = llvm::Type::getInt32Ty(*context_);
    globalTys_.arraySize = globalTys_.integer;
    globalTys_.heapObj = llvm::StructType::create(*context_, {
                                                                 globalTys_.refcSize /* ref_count */
                                                             },
                                                  "heap_obj");
    globalTys_.heapArrayObj = llvm::StructType::create(*context_, {
                                                                      globalTys_.refcSize /* ref_count */, globalTys_.arraySize /* fixed size */
                                                                  },
                                                       "arr_obj");
}

static bool systemToolExists(const std::string &tool)
{
    std::string cmd = tool + " --version 2>/dev/null";
    return system(cmd.c_str()) == 0;
}

int Codegen::configure(int *argc, char **argv)
{
    bool nextIsOutputFileName = false;
    for (int i = 1; i < *argc - 1; ++i)
    {
        std::string_view arg(argv[i]);

        if (nextIsOutputFileName)
        {
            config_.outputFileName = arg;
            nextIsOutputFileName = false;
            continue;
        }

        if (arg.size() > 1 &&
            "-G" == arg.substr(0, 2))
        {
            // Heap obj allocation data
            std::string_view option = arg.size() > 2 ? arg.substr(2) : "";
            if ("print-heap-management" == option)
            {
                config_.printHeapManagement = true;
            }
            else if ("b" == option)
            {
                config_.buildIntoExe = true;
            }
            else
            {
                std::cerr << "Unrecognized -G option: " << option << "\n";
                return 1;
            }
        }
        else if (arg.size() > 1 &&
                 "-o" == arg.substr(0, 2))
        {
            nextIsOutputFileName = true;
        }
    }

    if (nextIsOutputFileName)
    {
        std::cerr << "Expected a filename after -o\n";
        return 1;
    }

    if (config_.buildIntoExe)
    {
        if (!systemToolExists("gcc"))
        {
            std::cerr << "gcc not found on computer. Please, install it to be able to compile into an executable\n";
            return 1;
        }
        if (!systemToolExists("llc"))
        {
            std::cerr << "llc not found on computer. Please, install it to be able to compile into an executable\n";
            return 1;
        }

        if (config_.outputFileName.empty())
            config_.outputFileName = "a.out";
    }
    else
    {
        if (config_.outputFileName.empty())
            config_.outputFileName = "dump.ll";
    }

    return 0;
}

void Codegen::run()
{
    initMetaGlobals();
    genMetaFunctions();
    setupMainEntryPoint();
    genGlobalVars();
    genRoutines();
    build();
}

// ----------------------------------- gen -------------------------------------
// -----------------------------------------------------------------------------

llvm::Value *Codegen::gen(const ast::Var &node)
{
    if (globalScope_)
    {
        llvm::Type *llTy = getType(*node.type);
        GlobalVariable *llGlobalVar;

        if (analyzer::isPrimitiveType(analyzer::getPureType(*node.type)))
        {
            module_->getOrInsertGlobal(node.id, llTy);
            llGlobalVar = module_->getNamedGlobal(node.id);

            llvm::Constant *llInitializer = getConstInitializer(*node.val);
            llGlobalVar->setInitializer(llInitializer);
        }
        else
        {
            ast::Type &type = analyzer::getPureType(*node.type);
            if (type.code != TypeEnum::Array && type.code != TypeEnum::Record)
                llvm_unreachable("gen Var got a non-primitive type that is not Array nor Record");

            // if type is complex, store a ptr to its heap obj
            module_->getOrInsertGlobal(node.id, llTy->getPointerTo());
            llGlobalVar = module_->getNamedGlobal(node.id);
            llGlobalVar->setInitializer(
                llvm::ConstantPointerNull::get(llTy->getPointerTo()));

            llvm::Value *llPtr = newHeapObject(*node.type, llTy, *mainEntryBuilder_);
            mainEntryBuilder_->CreateStore(llPtr, llGlobalVar);

            if (blockStack_.empty())
                llvm_unreachable("no blockStack entries (globalVar)");
            auto &blockInfo = blockStack_.back();
            blockInfo.heapObjs.emplace_back(llGlobalVar, *node.type);
            std::cerr << "GLOBAL VAR ";
        }

        vars_.emplace(&node, VarMapping{llGlobalVar, nullptr});
        return llGlobalVar;
    }
    else
    {
        llvm::Function *llParentFn = builder_->GetInsertBlock()->getParent();
        llvm::IRBuilder<> tmpBuilder(
            &llParentFn->getEntryBlock(),
            llParentFn->getEntryBlock().begin());

        llvm::AllocaInst *llVar;
        if (analyzer::isPrimitiveType(analyzer::getPureType(*node.type)))
        {
            llvm::Value *llInitializer = node.val->codegen(*this);
            llVar = tmpBuilder.CreateAlloca(llInitializer->getType(), nullptr, node.id);
            builder_->CreateStore(llInitializer, llVar);
        }
        else
        {
            std::cerr << "getPureType\n";
            ast::Type &type = analyzer::getPureType(*node.type);
            if (type.code != TypeEnum::Array && type.code != TypeEnum::Record)
                llvm_unreachable("gen Var got a non-primitive type that is not Array nor Record");

            std::cerr << "getType\n";
            llvm::Type *llTy = getType(*node.type);
            llVar = tmpBuilder.CreateAlloca(llTy->getPointerTo(), nullptr, node.id);

            std::cerr << "nextstep\n";
            llvm::Value *llInitialzer;
            if (node.val)
            {
                llInitialzer = node.val->codegen(*this);

                if (node.val->code == ExprEnum::IdRef)
                {
                    // increment refcount of the object the access is shared to

                    if (config_.printHeapManagement)
                    {
                        builder_->CreateCall(module_->getFunction("printf"), builder_->CreateGlobalStringPtr("HEAP: var initialization...\n"));
                    }

                    heapObjUseCountInc(llInitialzer);
                }
            }
            else
            {
                llInitialzer = newHeapObject(*node.type, llTy, *builder_);
            }
            builder_->CreateStore(llInitialzer, llVar);

            if (blockStack_.empty())
                llvm_unreachable("no blockStack entries (localVar)");
            auto &blockInfo = blockStack_.back();
            blockInfo.heapObjs.emplace_back(llVar, *node.type);
        }
        vars_.emplace(&node, VarMapping{llVar, llParentFn});
        return llVar;
    }
}

llvm::Value *Codegen::gen(const ast::Routine &node)
{
    if (!node.body)
        llvm_unreachable("gen Routine got a routine with no body");

    isMainRoutine_ = (node.id == "main");
    std::cerr << "routine\n";
    Function *llFn;
    if (isMainRoutine_)
    {
        llFn = globals_.main;

        BasicBlock *llStartBlk = BasicBlock::Create(*context_, "start", llFn);
        mainEntryBuilder_->CreateBr(llStartBlk);
        // Add new instructions after the mainEntryBuilder_ position
        builder_->SetInsertPoint(llStartBlk);
    }
    else
    {
        std::cerr << "gentype\n";
        llvm::FunctionType *llFnTy = genRoutineType(*node.getType());
        llFn = llvm::Function::Create(llFnTy, llvm::Function::ExternalLinkage, node.id, module_.get());

        BasicBlock *llEntryBlk = BasicBlock::Create(*context_, "entry", llFn);
        builder_->SetInsertPoint(llEntryBlk);
    }
    std::cerr << "parameteres\n";

    if (!isMainRoutine_)
        blockStack_.emplace_back(std::list<HeapObj>{}, true);
    for (auto &llParam : llFn->args())
    {
        const int paramNo = llParam.getArgNo();
        const auto &param = node.getType()->params[paramNo];

        llvm::Type *llParamTy = llFn->getFunctionType()->getParamType(paramNo);
        if (!analyzer::isPrimitiveType(analyzer::getPureType(*param->type)))
            llParamTy = llParamTy->getPointerTo();
        std::cerr << "param\n";
        llvm::Value *llVar = builder_->CreateAlloca(llParamTy, nullptr, param->id);
        vars_.emplace(param.get(), VarMapping{llVar, llFn});

        builder_->CreateStore(&llParam, llVar);
        if (!analyzer::isPrimitiveType(analyzer::getPureType(*param->type)))
        {
            blockStack_.back().heapObjs.emplace_back(llVar, *param->type);

            if (config_.printHeapManagement)
            {
                builder_->CreateCall(module_->getFunction("printf"), builder_->CreateGlobalStringPtr("HEAP: function parameter...\n"));
            }

            heapObjUseCountInc(&llParam);
        }
    }

    codegenBlock(*node.body, true);
    std::cerr << "block genned\n";
    llvm::Instruction *last = !builder_->GetInsertBlock()->empty()
                                  ? &builder_->GetInsertBlock()->back()
                                  : nullptr;
    if (!last || !llvm::isa<llvm::ReturnInst>(last))
    {
        if (isMainRoutine_)
        {
            llvm::APInt llRetValInt(32 /* bitSize */, 0, true /* signed */);
            builder_->CreateRet(llvm::ConstantInt::get(*context_, llRetValInt));
        }
        else if (llFn->getReturnType()->isVoidTy())
        {
            builder_->CreateRetVoid();
        }
    }
    isMainRoutine_ = false;
    std::cerr << "try verify\n";
    llvm::verifyFunction(*llFn);
    std::cerr << "verified\n";
    return nullptr;
}

llvm::Value *Codegen::gen(const ast::TypeDecl &node)
{
    // nothing has to be done, all types have already been converted to their full form during Analyzer stage
    return nullptr;
}

llvm::Value *Codegen::gen(const ast::Assignment &node)
{
    if (globalScope_)
        llvm_unreachable("non-elided at compile-time assignment in global scope is illegal");
    std::cerr << "Assign left\n";
    llvm::Value *llLhsPtr = codegenPrimaryPtr(*node.left);
    std::cerr << "Assign left done\n";
    ast::Type &leftType = analyzer::getPureType(*node.left->type);
    std::cerr << "Assign right\n";
    llvm::Value *llRhs = node.val->codegen(*this);
    ast::Type &valType = analyzer::getPureType(*node.val->type);

    if (leftType.code == TypeEnum::Int)
    {
        if (valType.code == TypeEnum::Real)
        {
            llRhs = builder_->CreateFPToSI(llRhs, globalTys_.integer, "ftoi");
        }
        else if (valType.code == TypeEnum::Bool)
        {
            llRhs = builder_->CreateZExt(llRhs, globalTys_.integer, "btoi");
        }
    }
    else if (leftType.code == TypeEnum::Real)
    {
        if (valType.code == TypeEnum::Int)
        {
            convertToDouble(llRhs);
        }
        else if (valType.code == TypeEnum::Bool)
        {
            llRhs = builder_->CreateZExt(llRhs, globalTys_.integer, "btoi");
            convertToDouble(llRhs);
        }
    }
    else if (leftType.code == TypeEnum::Bool)
    {
        if (valType.code == TypeEnum::Int)
        {
            llRhs = builder_->CreateICmpNE(llRhs, llvm::ConstantInt::get(*context_, llvm::APInt(1, 0)), "itob");
        }
    }
    else if (leftType.code == TypeEnum::Array || leftType.code == TypeEnum::Record)
    {
        if (node.val->code == ExprEnum::IdRef)
        {
            // increment refcount of the object the access is shared to

            if (config_.printHeapManagement)
            {
                builder_->CreateCall(module_->getFunction("printf"), builder_->CreateGlobalStringPtr("HEAP: assignment...\n"));
            }

            heapObjUseCountInc(llRhs);
        }

        llvm::Value *llDeref = builder_->CreateLoad(globalTys_.heapObj->getPointerTo(), llLhsPtr);
        heapObjUseCountDecr(llDeref, *node.left->type);
    }
    else
        llvm_unreachable("gen Assignment: unexpected lhs type code");

    builder_->CreateStore(llRhs, llLhsPtr);
    std::cerr << "Assign done\n";
    return nullptr;
}

llvm::Value *Codegen::gen(const ast::PrintStmt &node)
{
    std::string fmtStr;
    fmtStr.resize(std::max(node.args.size() * 3 /* 3 chars for each param, including space */, 1UL));

    size_t i = 0;
    for (auto &arg : node.args)
    {
        fmtStr[i++] = '%';

        char c;
        switch (analyzer::getPureType(*arg->type).code)
        {
        case TypeEnum::Bool:
        {
            c = 's';
            break;
        }
        case TypeEnum::Int:
        {
            c = 'd';
            break;
        }
        case TypeEnum::Real:
        {
            c = 'f';
            break;
        }
        default:
            llvm_unreachable("gen PrintStmt: unexpected arg type");
        }

        fmtStr[i++] = c;
        if (i + 1 < fmtStr.size())
            fmtStr[i++] = ' ';
    }
    fmtStr[i] = '\n';

    llvm::Constant *llFmtStr = builder_->CreateGlobalStringPtr(fmtStr, ".fmt");

    std::vector<llvm::Value *> llArgs;
    llArgs.reserve(node.args.size() + 1);
    llArgs.push_back(llFmtStr);

    for (auto &arg : node.args)
    {
        llvm::Value *llArg = arg->codegen(*this);
        if (llArg == nullptr)
            throw std::runtime_error("print arg has returned nullptr from codegen");

        // Converting bool to string on output
        if (analyzer::getPureType(*arg->type).code == TypeEnum::Bool)
        {
            llArg = builder_->CreateSelect(llArg, globals_.strTrue, globals_.strFalse, "boolStr");
        }

        llArgs.push_back(llArg);
    }

    llvm::Function *llPrintf = module_->getFunction("printf");
    builder_->CreateCall(llPrintf, llArgs);

    return nullptr;
}

llvm::Value *Codegen::gen(const ast::IfStmt &node)
{
    std::cerr << "cond\n";
    llvm::Value *llCond = node.condition->codegen(*this);
    std::cerr << "condDONE\n";
    if (!llCond)
        llvm_unreachable("gen IfStmt: cond is null");

    llvm::Function *llParentFn = builder_->GetInsertBlock()->getParent();
    llvm::BasicBlock *llThenBlk = llvm::BasicBlock::Create(*context_, "then", llParentFn);
    llvm::BasicBlock *llElseBlk = node.elseBody
                                      ? llvm::BasicBlock::Create(*context_, "else", llParentFn)
                                      : nullptr;
    llvm::BasicBlock *llContBlk = llvm::BasicBlock::Create(*context_, "ifcont", llParentFn);

    builder_->CreateCondBr(llCond, llThenBlk, llElseBlk ? llElseBlk : llContBlk);

    builder_->SetInsertPoint(llThenBlk);
    codegenBlock(*node.body, false);
    llvm::Instruction *last = !builder_->GetInsertBlock()->empty()
                                  ? &builder_->GetInsertBlock()->back()
                                  : nullptr;
    if (!last || !llvm::isa<llvm::ReturnInst>(last))
    {
        builder_->CreateBr(llContBlk);
    }

    if (llElseBlk)
    {
        builder_->SetInsertPoint(llElseBlk);
        codegenBlock(*node.elseBody, false);
        llvm::Instruction *last = !builder_->GetInsertBlock()->empty()
                                      ? &builder_->GetInsertBlock()->back()
                                      : nullptr;
        if (!last || !llvm::isa<llvm::ReturnInst>(last))
        {
            builder_->CreateBr(llContBlk);
        }
    }

    builder_->SetInsertPoint(llContBlk);

    return nullptr;
}

llvm::Value *Codegen::gen(const ast::ForStmt &node)
{
    std::cerr << "for stmt\n";
    llvm::Function *llParentFn = builder_->GetInsertBlock()->getParent();
    llvm::BasicBlock *llLoopInitBlk = llvm::BasicBlock::Create(*context_, "loopinit", llParentFn);
    llvm::BasicBlock *llLoopBlk = llvm::BasicBlock::Create(*context_, "loop", llParentFn);
    llvm::BasicBlock *llLoopBodyBlk = llvm::BasicBlock::Create(*context_, "loopbody", llParentFn);
    llvm::BasicBlock *llBrkBlk = llvm::BasicBlock::Create(*context_, "loopbrk", llParentFn);

    llvm::Value *llStart, *llEnd;
    auto *intRange = dynamic_cast<IntRange *>(node.range.get());
    if (intRange)
    {
        llStart = intRange->start->codegen(*this);
        llEnd = intRange->end->codegen(*this);
    }
    else
    {
        auto *arrayIdRange = dynamic_cast<ArrayIdRange *>(node.range.get());
        if (!arrayIdRange)
            llvm_unreachable("gen ForStmt: unexpected range");
        llStart = llvm::ConstantInt::get(globalTys_.integer, 0);

        // load array size RTTI
        auto varDecl = arrayIdRange->ref.lock();
        if (!varDecl)
            llvm_unreachable("gen ForStmt: ArrayIdRange ref is null");

        auto it = vars_.find(varDecl.get());
        if (it == vars_.end())
            llvm_unreachable("gen ForStmt: ArrayIdRange decl not in map");

        llvm::Value *llDeref = builder_->CreateLoad(globalTys_.heapArrayObj->getPointerTo(), it->second.llVarAllocation);
        llvm::Value *llSizeRttiPtr = builder_->CreateStructGEP(globalTys_.heapArrayObj, llDeref, 1);
        llEnd = builder_->CreateLoad(globalTys_.arraySize, llSizeRttiPtr);
        llEnd = builder_->CreateSExt(llEnd, globalTys_.integer);
        llEnd = builder_->CreateSub(llEnd, llvm::ConstantInt::get(globalTys_.integer, 1));
    }

    llvm::Value *llCond;
    if (node.reverse)
    {
        llCond = builder_->CreateICmpSGE(llEnd, llStart, "reverse_for_ck");
    }
    else
    {
        llCond = builder_->CreateICmpSLE(llStart, llEnd, "for_ck");
    }
    builder_->CreateCondBr(llCond, llLoopInitBlk, llBrkBlk);

    builder_->SetInsertPoint(llLoopInitBlk);
    std::cerr << "create alloca\n";
    llvm::Value *llVar = builder_->CreateAlloca(globalTys_.integer, nullptr, node.counter->id);
    vars_.emplace(node.counter.get(), VarMapping{llVar, llParentFn});

    if (node.reverse)
        builder_->CreateStore(llEnd, llVar);
    else
        builder_->CreateStore(llStart, llVar);
    std::cerr << "init done\n";
    builder_->CreateBr(llLoopBodyBlk);

    builder_->SetInsertPoint(llLoopBlk);
    llvm::Value *llCntVal = builder_->CreateLoad(globalTys_.integer, llVar, "for_cnt");
    if (node.reverse)
    {
        llvm::Value *llStep = builder_->CreateSub(llCntVal, llvm::ConstantInt::get(globalTys_.integer, 1));
        builder_->CreateStore(llStep, llVar, "for_step");
        llCond = builder_->CreateICmpSGE(llStep, llStart, "reverse_for_ck");
    }
    else
    {
        llvm::Value *llStep = builder_->CreateAdd(llCntVal, llvm::ConstantInt::get(globalTys_.integer, 1));
        builder_->CreateStore(llStep, llVar, "for_step");
        llCond = builder_->CreateICmpSLE(llStep, llEnd, "for_ck");
    }
    builder_->CreateCondBr(llCond, llLoopBodyBlk, llBrkBlk);

    builder_->SetInsertPoint(llLoopBodyBlk);
    codegenBlock(*node.body, false);
    llvm::Instruction *last = !builder_->GetInsertBlock()->empty()
                                  ? &builder_->GetInsertBlock()->back()
                                  : nullptr;
    if (!last || !llvm::isa<llvm::ReturnInst>(last))
    {
        builder_->CreateBr(llLoopBlk);
    }

    builder_->SetInsertPoint(llBrkBlk);

    return nullptr;
}

llvm::Value *Codegen::gen(const ast::WhileStmt &node)
{
    llvm::Function *llParentFn = builder_->GetInsertBlock()->getParent();
    llvm::BasicBlock *llLoopBlk = llvm::BasicBlock::Create(*context_, "loop", llParentFn);
    llvm::BasicBlock *llLoopBodyBlk = llvm::BasicBlock::Create(*context_, "loopbody", llParentFn);
    llvm::BasicBlock *llBrkBlk = llvm::BasicBlock::Create(*context_, "loopbrk", llParentFn);

    builder_->CreateBr(llLoopBlk);
    builder_->SetInsertPoint(llLoopBlk);

    llvm::Value *llCond = node.condition->codegen(*this);
    if (!llCond)
        llvm_unreachable("gen WhileStmt: cond is null");
    builder_->CreateCondBr(llCond, llLoopBodyBlk, llBrkBlk);

    builder_->SetInsertPoint(llLoopBodyBlk);
    codegenBlock(*node.body, false);
    llvm::Instruction *last = !builder_->GetInsertBlock()->empty()
                                  ? &builder_->GetInsertBlock()->back()
                                  : nullptr;
    if (!last || !llvm::isa<llvm::ReturnInst>(last))
    {
        builder_->CreateBr(llLoopBlk);
    }

    builder_->SetInsertPoint(llBrkBlk);

    return nullptr;
}

llvm::Value *Codegen::gen(const ast::ReturnStmt &node)
{
    if (isMainRoutine_)
    {
        llvm::APInt llRetValInt(32, 0, true);
        builder_->CreateRet(llvm::ConstantInt::get(*context_, llRetValInt));
    }
    else
    {
        llvm::Value *llRetVal;
        bool isRetValPrimitiveType;

        if (node.val)
        {
            isRetValPrimitiveType = analyzer::isPrimitiveType(analyzer::getPureType(*node.val->type));
            if (isRetValPrimitiveType)
            {
                llRetVal = node.val->codegen(*this);
            }
            else
            {
                // Get ptr to heap object, not the object itself, to compare to heapObjs.llPtr
                llRetVal = codegenPrimaryPtr(*node.val);
            }
        }

        bool hasHeapObjs = false;
        for (auto it = blockStack_.rbegin(); it != blockStack_.rend(); ++it)
        {
            if (!it->heapObjs.empty()
                // edge-case: if it's the value we return, we don't decrement its refc, and thus we don't need to create the branch
                && (it != blockStack_.rbegin() || it->heapObjs.size() > 1 || it->heapObjs.back().llPtr != llRetVal))
            {
                hasHeapObjs = true;
                break;
            }
            if (it->isFunction)
                break;
        }
        if (hasHeapObjs)
        {
            // Calling refc_dcr on all objects in the current scope of the function
            llvm::Function *llParentFn = builder_->GetInsertBlock()->getParent();
            BasicBlock *llClosingBlk = BasicBlock::Create(*context_, "return_destructor", llParentFn);
            builder_->CreateBr(llClosingBlk);
            builder_->SetInsertPoint(llClosingBlk);

            if (config_.printHeapManagement)
            {
                builder_->CreateCall(module_->getFunction("printf"), builder_->CreateGlobalStringPtr("HEAP: calling return destructor...\n"));
            }

            // Call decrement on all heap objects created in this block AND ones above AS LONG AS the blocks belong to this function
            for (auto it = blockStack_.rbegin(); it != blockStack_.rend(); ++it)
            {
                for (auto &heapObj : it->heapObjs)
                {
                    // Don't decrement the refc of the object being returned
                    if (heapObj.llPtr != llRetVal)
                    {
                        llvm::Value *llDeref = builder_->CreateLoad(globalTys_.heapObj->getPointerTo(), heapObj.llPtr);

                        heapObjUseCountDecr(llDeref, heapObj.type);
                    }
                }
                if (it->isFunction)
                    break;
            }

            BasicBlock *llBackBlk = BasicBlock::Create(*context_, "back", llParentFn);
            builder_->CreateBr(llBackBlk);
            builder_->SetInsertPoint(llBackBlk);

            // clear just in case there are unexpected statements after the return stmt (bug of Analyzer)
            // to not emit a double free at the end of this block
            blockStack_.back().heapObjs.clear();
        }
        std::cerr << "closeRRR\n";
        if (node.val)
        {
            if (!isRetValPrimitiveType)
            {
                llvm::Type *llRetTy = getType(*node.val->type)->getPointerTo();
                llRetVal = builder_->CreateLoad(llRetTy, llRetVal);
            }
            std::cerr << "ALMOST RETURN\n";
            builder_->CreateRet(llRetVal);
        }
        else
        {
            builder_->CreateRetVoid();
        }
    }

    std::cerr << blockStack_.back().heapObjs.size() << " " << blockStack_.back().heapObjs.empty() << "\n";
    return nullptr;
}

llvm::Value *Codegen::gen(const ast::Expr &node)
{
    switch (node.code)
    {
    case ExprEnum::BoolLiteral:
        return ConstantInt::get(
            llvm::Type::getInt1Ty(*context_),
            static_cast<const BoolLiteral &>(node).val);
    case ExprEnum::IntLiteral:
        return ConstantInt::get(
            globalTys_.integer,
            static_cast<const IntLiteral &>(node).val);
    case ExprEnum::RealLiteral:
        return ConstantFP::get(
            globalTys_.real,
            static_cast<const RealLiteral &>(node).val);
    }
    llvm_unreachable("Codegen Expr: unexpected code " + std::to_string(static_cast<int>(node.code)));
}

llvm::Value *Codegen::gen(const ast::BinaryExpr &node)
{
    llvm::Value *llLeft = node.left->codegen(*this);
    if (llLeft == nullptr)
        throw std::runtime_error("Codegen BinaryExpr left is nullptr");
    llvm::Value *llRight = node.right->codegen(*this);
    if (llRight == nullptr)
        throw std::runtime_error("Codegen BinaryExpr right is nullptr");

    ast::Type &type = analyzer::getPureType(*node.type);
    ast::Type &leftType = analyzer::getPureType(*node.left->type);
    ast::Type &rightType = analyzer::getPureType(*node.right->type);

    switch (node.code)
    {
    case ExprEnum::Add:
    {
        if (type.code == TypeEnum::Int)
        {
            return builder_->CreateAdd(llLeft, llRight, "add");
        }
        else if (type.code == TypeEnum::Real)
        {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFAdd(llLeft, llRight, "fadd");
        }
        else
            break;
    }
    case ExprEnum::Subtract:
    {
        if (type.code == TypeEnum::Int)
        {
            return builder_->CreateSub(llLeft, llRight, "sub");
        }
        else if (type.code == TypeEnum::Real)
        {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFSub(llLeft, llRight, "fsub");
        }
        else
            break;
    }
    case ExprEnum::Multiply:
    {
        if (type.code == TypeEnum::Int)
        {
            return builder_->CreateMul(llLeft, llRight, "mul");
        }
        else if (type.code == TypeEnum::Real)
        {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFMul(llLeft, llRight, "fmul");
        }
        else
            break;
    }
    case ExprEnum::Divide:
    {
        if (type.code == TypeEnum::Int)
        {
            return builder_->CreateSDiv(llLeft, llRight, "div");
        }
        else if (type.code == TypeEnum::Real)
        {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFDiv(llLeft, llRight, "fdiv");
        }
        else
            break;
    }
    case ExprEnum::Modulo:
    {
        if (type.code == TypeEnum::Int)
        {
            return builder_->CreateSRem(llLeft, llRight, "rem");
        }
        else
            break;
    }
    case ExprEnum::And:
        return builder_->CreateAnd(llLeft, llRight, "and");
    case ExprEnum::Or:
        return builder_->CreateOr(llLeft, llRight, "or");
    case ExprEnum::Xor:
        return builder_->CreateXor(llLeft, llRight, "xor");
    case ExprEnum::LESS_THAN:
    {
        if (leftType.code == TypeEnum::Real || rightType.code == TypeEnum::Real)
        {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFCmpOLT(llLeft, llRight, "flt");
        }
        return builder_->CreateICmpSLT(llLeft, llRight, "lt");
    }
    case ExprEnum::LESS_OR_EQUAL:
    {
        if (leftType.code == TypeEnum::Real || rightType.code == TypeEnum::Real)
        {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFCmpOLE(llLeft, llRight, "fle");
        }
        return builder_->CreateICmpSLE(llLeft, llRight, "le");
    }
    case ExprEnum::MORE_THAN:
    {
        if (leftType.code == TypeEnum::Real || rightType.code == TypeEnum::Real)
        {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFCmpOGT(llLeft, llRight, "fgt");
        }
        return builder_->CreateICmpSGT(llLeft, llRight, "gt");
    }
    case ExprEnum::MORE_OR_EQUAL:
    {
        if (leftType.code == TypeEnum::Real || rightType.code == TypeEnum::Real)
        {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFCmpOGE(llLeft, llRight, "fge");
        }
        return builder_->CreateICmpSGE(llLeft, llRight, "ge");
    }
    case ExprEnum::EQUAL:
    {
        if (leftType.code == TypeEnum::Real || rightType.code == TypeEnum::Real)
        {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFCmpOEQ(llLeft, llRight, "feq");
        }
        return builder_->CreateICmpEQ(llLeft, llRight, "eq");
    }
    case ExprEnum::UNEQUAL:
    {
        if (leftType.code == TypeEnum::Real || rightType.code == TypeEnum::Real)
        {
            convertToDouble(llLeft);
            convertToDouble(llRight);
            return builder_->CreateFCmpONE(llLeft, llRight, "fne");
        }
        return builder_->CreateICmpNE(llLeft, llRight, "ne");
    }
    }
    llvm_unreachable("Codegen BinaryExpr: unexpected code " + std::to_string(static_cast<int>(node.code)));
}

llvm::Value *Codegen::gen(const ast::UnaryExpr &node)
{
    llvm::Value *llVal = node.val->codegen(*this);
    if (llVal == nullptr)
        throw std::runtime_error("Codegen UnaryExpr: val is nullptr");

    switch (node.code)
    {
    case ExprEnum::Negate:
        return builder_->CreateNeg(llVal, "neg");
    case ExprEnum::Not:
        return builder_->CreateNot(llVal, "not");
    }
    llvm_unreachable("Codegen UnaryExpr: unexpected code " + std::to_string(static_cast<int>(node.code)));
}

llvm::Value *Codegen::gen(const ast::IdRef &node)
{
    auto varDecl = node.ref.lock();
    if (!varDecl)
        llvm_unreachable("Codegen IdRef does not refer to any declaration");

    auto it = vars_.find(varDecl.get());
    if (it == vars_.end())
        llvm_unreachable("Codegen IdRef ref is not yet added to vars map: " + node.id);

    llvm::Value *llVar = it->second.llVarAllocation;
    llvm::Type *llTy = getType(*varDecl->type); // pointers are opaque, so we need to get the type from AST again
    if (node.next)
    {
        primaryType_ = &analyzer::getPureType(*varDecl->type);
        llPrimaryTy_ = llTy;

        llvm::Value *llDerefVar = builder_->CreateLoad(llPrimaryTy_->getPointerTo(), llVar);
        llPrimaryPtr_ = llDerefVar;
        llvm::Value *llDescendantPtr = node.next->codegen(*this);
        llPrimaryPtr_ = nullptr;
        if (getVarPtr_)
        {
            // Get ptr
            return llDescendantPtr;
        }

        // Get value
        std::cerr << "get value type code: " << static_cast<int>(primaryType_->code) << "\n";
        llvm::Type *llDescendantTy = getType(*primaryType_);
        if (!analyzer::isPrimitiveType(analyzer::getPureType(*primaryType_)))
            llDescendantTy = llDescendantTy->getPointerTo();
        llvm::Value *llLoad = builder_->CreateLoad(llDescendantTy, llDescendantPtr);
        return llLoad;
    }

    if (getVarPtr_)
        return llVar;

    if (!analyzer::isPrimitiveType(analyzer::getPureType(*node.type)))
        llTy = llTy->getPointerTo();
    llvm::Value *llLoad = builder_->CreateLoad(llTy, llVar);
    return llLoad;
}

llvm::Value *Codegen::gen(const ast::RecordMember &node)
{
    if (llPrimaryPtr_ == nullptr)
        llvm_unreachable("Codegen RecordMember: rogue");

    ast::Type &prType = analyzer::getPureType(*primaryType_);
    std::cerr << "PureType code: " << node.id << " " << static_cast<int>(prType.code) << "\n";
    if (prType.code != TypeEnum::Record)
    {
        if (prType.code == TypeEnum::Array && node.id == "size")
        {
            auto it = typeArrayHashMap_.find(llPrimaryTy_);
            if (it == typeArrayHashMap_.end())
                llvm_unreachable("gen RecordMember: array type not in map");
            std::cerr << "DUMPFDFDF\n";

            llvm::ArrayType *llArrayTy = it->second;
            llvm::Value *llSizeRttiPtr = builder_->CreateStructGEP(globalTys_.heapArrayObj, llPrimaryPtr_, 1);

            primaryType_ = ast_->getBaseTypes().integer.get();
            llPrimaryTy_ = globalTys_.arraySize;
            return llSizeRttiPtr;
        }
        llvm_unreachable("Codegen RecordMember: parent type is not a record");
    }

    std::cerr << "its a record " << analyzer::stringifyType(prType) << "\n";
    auto &members = static_cast<RecordType &>(prType).members;
    std::cerr << "got members " << members.size() << "\n";
    size_t offset = 0;
    ast::Type *fieldType;
    for (auto &mem : members)
    {
        if (mem->id == node.id)
        {
            fieldType = &analyzer::getPureType(*mem->type);
            break;
        }
        offset++;
    }

    if (offset == members.size())
        llvm_unreachable("Codegen RecordMember: member not found by name");

    // Return ptr to this field
    llvm::Value *llFieldPtr = builder_->CreateStructGEP(llPrimaryTy_, llPrimaryPtr_,
                                                        offset + 1 /* +1 to skip over ref_count */);
    std::cerr << "written field\n";
    primaryType_ = fieldType;

    if (node.next)
    {
        std::cerr << "next from RecordMember\n";
        llPrimaryTy_ = getType(*primaryType_);
        llPrimaryPtr_ = builder_->CreateLoad(llPrimaryTy_->getPointerTo(), llFieldPtr);
        return node.next->codegen(*this);
    }
    return llFieldPtr;
}

llvm::Value *Codegen::gen(const ast::ArrayAccess &node)
{
    if (llPrimaryPtr_ == nullptr)
        llvm_unreachable("gen ArrayAccess: rogue");

    ast::Type &prType = analyzer::getPureType(*primaryType_);
    if (prType.code != TypeEnum::Array)
        llvm_unreachable("gen ArrayAccess: parent type is not an array");

    // temporarily setting getVarPtr_ to false so that array index expr retuns us a value, not a ptr
    bool prevGetVarPtr = getVarPtr_;
    getVarPtr_ = false;
    llvm::Value *prevPrimaryPtr{llPrimaryPtr_};
    llvm::Type *prevPrimaryTy_{llPrimaryTy_};

    std::cerr << "get offse\n";
    llvm::Value *llOffset = node.val->codegen(*this);
    if (llOffset == nullptr)
        llvm_unreachable("gen ArrayAccess: val is null");
    if (!llOffset->getType()->isIntegerTy(64))
        llvm_unreachable("gen ArrayAccess: expr type is not i64");

    getVarPtr_ = prevGetVarPtr;
    llPrimaryPtr_ = prevPrimaryPtr;
    llPrimaryTy_ = prevPrimaryTy_;

    // truncate to i32
    llOffset = builder_->CreateTruncOrBitCast(llOffset, globalTys_.arraySize);
    std::cerr << "get pure type change\n";
    primaryType_ = &analyzer::getPureType(*static_cast<ast::ArrayType &>(prType).elemType);
    std::cerr << "changed\n";

    llvm::Value *llArrayBasePtr = builder_->CreateStructGEP(llPrimaryTy_, llPrimaryPtr_,
                                                            2 /* skip over meta info */
    );
    std::cerr << "abb\n";

    auto it = typeArrayHashMap_.find(llPrimaryTy_);
    if (it == typeArrayHashMap_.end())
        llvm_unreachable("gen ArrayAccess: no array type inside hash map");
    std::cerr << "abcsdsad\n";

    // Check in-bounds
    builder_->CreateCall(globals_.arr_bnds_ck, {llPrimaryPtr_, llOffset});

    // Return ptr to this element
    llvm::Value *llElemPtr = builder_->CreateGEP(it->second, llArrayBasePtr,
                                                 {
                                                     llvm::ConstantInt::get(*context_, llvm::APInt(32, 0)),
                                                     llOffset,
                                                 });
    std::cerr << "check next\n";

    if (node.next)
    {
        std::cerr << "node next ArrayAcces\n";
        llPrimaryTy_ = getType(*primaryType_);
        llvm::Value *llLoad = builder_->CreateLoad(llPrimaryTy_->getPointerTo(), llElemPtr);
        llPrimaryPtr_ = llLoad;
        return node.next->codegen(*this);
    }
    return llElemPtr;
}

llvm::Value *Codegen::gen(const ast::RoutineCall &node)
{
    std::vector<llvm::Value *> llArgs;
    llArgs.reserve(node.args.size());

    bool prevGetVarPtr = getVarPtr_;
    getVarPtr_ = false;
    llvm::Value *prevPrimaryPtr{llPrimaryPtr_};
    llvm::Type *prevPrimaryTy_{llPrimaryTy_};

    for (auto &arg : node.args)
    {
        llvm::Value *llArg = arg->codegen(*this);
        llArgs.push_back(llArg);
    }

    llPrimaryPtr_ = prevPrimaryPtr;
    llPrimaryTy_ = prevPrimaryTy_;
    getVarPtr_ = prevGetVarPtr;

    llvm::Function *llFn = module_->getFunction(node.routineId);
    if (!llFn)
        llvm_unreachable("gen RoutineCall: no function found");

    llFn->dump();
    std::cerr << "CALLING\n";
    for (auto &a : llArgs)
        a->dump();
    llvm::Value *llCall = builder_->CreateCall(llFn, llArgs);
    std::cerr << "Call happened\n";
    // If the expr is not set to a variable and it returns a heap object, defer its deletion to the end of the unit in Block
    if (!node.isNamed)
    {
        auto routineDecl = node.ref.lock();
        if (routineDecl == nullptr)
            llvm_unreachable("RoutineCall ref is null");

        const auto &retType = routineDecl->getType()->retType;
        if (retType && !analyzer::isPrimitiveType(analyzer::getPureType(*retType)))
        {
            std::cerr << "emp;\n";
            tmpHeapObjects_.emplace_back(llCall, analyzer::getPureType(*retType));
        }
        std::cerr << "nexte\n";
    }
    std::cerr << "after tmps\n";
    if (node.next)
    {
        auto routineDecl = node.ref.lock();
        if (routineDecl == nullptr)
            llvm_unreachable("RoutineCall ref is null");
        if (!routineDecl->getType()->retType)
            llvm_unreachable("RoutineCall has next but no return type");

        primaryType_ = &analyzer::getPureType(*routineDecl->getType()->retType);
        llPrimaryTy_ = getType(*primaryType_);

        llPrimaryPtr_ = llCall;
        llvm::Value *llDescendantPtr = node.next->codegen(*this);
        llPrimaryPtr_ = nullptr;

        if (getVarPtr_)
        {
            // Get ptr
            return llDescendantPtr;
        }

        // Get value
        llvm::Type *llMemberTy = getType(*primaryType_);
        llvm::Value *llLoad = builder_->CreateLoad(llMemberTy, llDescendantPtr);
        return llLoad;
    }
    std::cerr << "Call return\n";
    return llCall;
}

// --------------------------------- genType -----------------------------------
// -----------------------------------------------------------------------------

llvm::Type *Codegen::genType(const ast::Type &node)
{
    switch (analyzer::getPureType(node).code)
    {
    case TypeEnum::Bool:
        return llvm::Type::getInt1Ty(*context_);
    case TypeEnum::Int:
        return globalTys_.integer;
    case TypeEnum::Real:
        return globalTys_.real;
    default:
        llvm_unreachable("genType on Type class called with invalid code " + std::to_string(static_cast<int>(node.code)));
    }
}

llvm::Type *Codegen::genType(const ast::ArrayType &node)
{
    llvm::Type *llElemTy = getType(*node.elemType);
    if (llElemTy->isStructTy() || llElemTy->isArrayTy())
    {
        // If element type is complex, array will store pointers to heap-allocated objects
        llElemTy = llElemTy->getPointerTo();
    }

    int64_t size;
    if (node.size)
    {
        llvm::Constant *llSize = getConstInitializer(*node.size);
        size = static_cast<llvm::ConstantInt *>(llSize)->getSExtValue();
    }
    else
    {
        size = 0;
    }
    std::cerr << "arr type\n";
    llvm::ArrayType *llArrayTy = llvm::ArrayType::get(llElemTy, size);
    llvm::Type *llTy = StructType::create({globalTys_.refcSize,
                                           globalTys_.arraySize,
                                           // static array itself
                                           llArrayTy});
    std::cerr << "emplace\n";
    typeArrayHashMap_.emplace(llTy, llArrayTy);
    return llTy;
}

llvm::Type *Codegen::genType(const ast::RecordType &node)
{
    std::vector<llvm::Type *> members;
    members.reserve(node.members.size() + 1);
    members.push_back(globalTys_.refcSize);

    for (auto &member : node.members)
    {
        llvm::Type *llMemberTy = getType(*member->type);
        if (llMemberTy->isStructTy() || llMemberTy->isArrayTy())
        {
            // If type is complex, the parent struct will store a ptr to heap-allocated object
            members.push_back(llMemberTy->getPointerTo());
        }
        else
        {
            members.push_back(llMemberTy);
        }
    }

    llvm::StructType *llTy = StructType::create(members);
    return llTy;
}

// Reusing type string hashes to reuse previously created llvm::Type's
llvm::Type *Codegen::getType(const ast::Type &node)
{
    std::stringstream ss;
    const ast::Type &pureType = analyzer::getPureType(node);
    pureType.serializeType({.os = ss, .ir = true});
    std::cerr << "slrzed type: " << ss.str() << "\n";
    auto typeIt = typeHashMap_.find(ss.str());
    if (typeIt != typeHashMap_.end())
    {
        std::cerr << "ret type\n";
        return typeIt->second;
    }
    std::cerr << "codegenType\n";
    llvm::Type *llTy = pureType.codegenType(*this);
    typeHashMap_.emplace(std::move(ss).str(), llTy);
    std::cerr << "done" << llTy << "\n";
    std::cerr << "returning type\n";
    return llTy;
}

// ------------------------------ Private methods ------------------------------
// -----------------------------------------------------------------------------

// LLVM builder CreateGlobalStringPtr doesn't work outside of insertion block, hence done manually
llvm::Constant *Codegen::newGlobalStrGlobalScope(const char *str, const char *label)
{
    auto *llData = llvm::ConstantDataArray::getString(*context_, str, true /* null terminator */);
    auto *llGlobal = new llvm::GlobalVariable(
        *module_, llData->getType(), true,
        llvm::GlobalValue::PrivateLinkage, llData, label);
    llGlobal->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

    auto *llZero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0);
    llvm::Constant *indices[] = {llZero, llZero};

    return llvm::ConstantExpr::getGetElementPtr(llData->getType(), llGlobal, indices);
}

void Codegen::initMetaGlobals()
{
    module_->getOrInsertFunction(
        "printf",
        llvm::FunctionType::get(
            llvm::IntegerType::getInt32Ty(*context_),
            llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(*context_)),
            true /* vararg */
            ));

    llvm::Type *ptrSizeTy = module_->getDataLayout().getIntPtrType(*context_, /*AddressSpace=*/0);
    module_->getOrInsertFunction(
        "malloc",
        llvm::FunctionType::get(
            llvm::Type::getInt8Ty(*context_)->getPointerTo(),
            ptrSizeTy,
            false));

    module_->getOrInsertFunction(
        "free",
        llvm::FunctionType::get(
            llvm::Type::getVoidTy(*context_),
            llvm::Type::getInt8Ty(*context_)->getPointerTo(),
            false));

    module_->getOrInsertFunction(
        "abort",
        llvm::FunctionType::get(
            llvm::Type::getVoidTy(*context_),
            false));

    globals_.strTrue = newGlobalStrGlobalScope("true", ".str_true");
    globals_.strFalse = newGlobalStrGlobalScope("false", ".str_false");
}

void Codegen::setupMainEntryPoint()
{
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

void Codegen::genMetaFunctions()
{
    /* .refc_inc */
    {
        llvm::FunctionType *llFnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*context_),
            {globalTys_.heapObj->getPointerTo()},
            false /* isVarArgs */
        );

        llvm::Function *llFn = llvm::Function::Create(llFnTy, llvm::Function::ExternalLinkage, ".refc_inc", module_.get());
        globals_.refc_inc = llFn;

        BasicBlock *llEntryBlk = BasicBlock::Create(*context_, "entry", llFn);
        builder_->SetInsertPoint(llEntryBlk);

        if (config_.printHeapManagement)
        {
            builder_->CreateCall(module_->getFunction("printf"), builder_->CreateGlobalStringPtr("***HEAP: refc_inc\n"));
        }

        // Implementation
        llvm::Argument *llPtr = llFn->getArg(0);
        llvm::Value *llRefcntPtr = builder_->CreateStructGEP(
            globalTys_.heapObj,
            llPtr, 0);

        llvm::Value *llRefcntVal = builder_->CreateLoad(globalTys_.refcSize, llRefcntPtr, "refcnt");
        llvm::Value *llNewRefcnt = builder_->CreateAdd(llRefcntVal, llvm::ConstantInt::get(*context_, llvm::APInt(32, 1)));
        builder_->CreateStore(llNewRefcnt, llRefcntPtr);

        builder_->CreateRetVoid();
    }
    /* .refc_dcr */
    {
        llvm::FunctionType *llFnTy = llvm::FunctionType::get(
            llvm::Type::getInt1Ty(*context_),
            {globalTys_.heapObj->getPointerTo()},
            false /* isVarArgs */
        );

        llvm::Function *llFn = llvm::Function::Create(llFnTy, llvm::Function::ExternalLinkage, ".refc_dcr", module_.get());
        globals_.refc_dcr = llFn;

        BasicBlock *llEntryBlk = BasicBlock::Create(*context_, "entry", llFn);
        builder_->SetInsertPoint(llEntryBlk);

        // Implementation
        llvm::Argument *llPtr = llFn->getArg(0);
        llvm::Value *llRefcntPtr = builder_->CreateStructGEP(
            globalTys_.heapObj,
            llPtr, 0);

        llvm::Function *llParentFn = builder_->GetInsertBlock()->getParent();
        llvm::BasicBlock *llFreeBlk = llvm::BasicBlock::Create(*context_, "free", llParentFn);
        llvm::BasicBlock *llDecrBlk = llvm::BasicBlock::Create(*context_, "dcr", llParentFn);
        llvm::BasicBlock *llContBlk = llvm::BasicBlock::Create(*context_, "continue", llParentFn);

        llvm::Value *llRefcntVal = builder_->CreateLoad(globalTys_.refcSize, llRefcntPtr, "refcnt");
        llvm::Value *llCmp = builder_->CreateICmpSLE(llRefcntVal, llvm::ConstantInt::get(*context_, llvm::APInt(32, 1)));
        builder_->CreateCondBr(llCmp, llFreeBlk, llDecrBlk);

        // free
        builder_->SetInsertPoint(llFreeBlk);

        if (config_.printHeapManagement)
        {
            builder_->CreateCall(module_->getFunction("printf"), builder_->CreateGlobalStringPtr("HEAP: object should be freed...\n"));
        }
        builder_->CreateRet(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), true));

        // dcr
        builder_->SetInsertPoint(llDecrBlk);
        auto *llNewRefcnt = builder_->CreateSub(llRefcntVal, llvm::ConstantInt::get(globalTys_.refcSize, 1));
        builder_->CreateStore(llNewRefcnt, llRefcntPtr);
        if (config_.printHeapManagement)
        {
            builder_->CreateCall(module_->getFunction("printf"), builder_->CreateGlobalStringPtr("***HEAP: refc_decr\n"));
        }
        builder_->CreateBr(llContBlk);

        // continue
        builder_->SetInsertPoint(llContBlk);
        builder_->CreateRet(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), false));
    }
    /* arr_bnds_ck */
    std::cerr << "init1\n";
    {
        llvm::FunctionType *llFnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*context_),
            {
                globalTys_.heapObj->getPointerTo(), // ptr
                globalTys_.arraySize                // offset
            },
            false /* isVarArgs */
        );

        llvm::Function *llFn = llvm::Function::Create(llFnTy, llvm::Function::ExternalLinkage, ".arr_bnds_ck", module_.get());
        globals_.arr_bnds_ck = llFn;

        BasicBlock *llEntryBlk = BasicBlock::Create(*context_, "entry", llFn);
        builder_->SetInsertPoint(llEntryBlk);

        // Implementation
        llvm::Argument *llPtr = llFn->getArg(0);
        llvm::Argument *llOffset = llFn->getArg(1);

        llvm::Value *llSizeRttiPtr = builder_->CreateStructGEP(
            globalTys_.heapArrayObj,
            llPtr, 1);
        llvm::Value *llArraySize = builder_->CreateLoad(globalTys_.arraySize, llSizeRttiPtr);
        // llvm::Type* ptrSizeTy = module_->getDataLayout().getIntPtrType(*context_, /*AddressSpace=*/0);
        // llArraySize = builder_->CreateSExt(llArraySize, ptrSizeTy);

        llvm::BasicBlock *llSecondCheckBlk = llvm::BasicBlock::Create(*context_, "ck_less", llFn);
        llvm::BasicBlock *llOkBlk = llvm::BasicBlock::Create(*context_, "ok", llFn);
        llvm::BasicBlock *llOutOfBoundsBlk = llvm::BasicBlock::Create(*context_, "out_of_bounds", llFn);

        llvm::Value *llCond = builder_->CreateICmpSGE(llOffset, llArraySize, "ge_ck");
        builder_->CreateCondBr(llCond, llOutOfBoundsBlk, llSecondCheckBlk);

        builder_->SetInsertPoint(llSecondCheckBlk);
        llCond = builder_->CreateICmpSLT(llOffset, llvm::ConstantInt::get(globalTys_.arraySize, 0), "lt_ck");
        builder_->CreateCondBr(llCond, llOutOfBoundsBlk, llOkBlk);

        builder_->SetInsertPoint(llOkBlk);
        builder_->CreateRetVoid();

        builder_->SetInsertPoint(llOutOfBoundsBlk);
        llvm::Function *llPrintf = module_->getFunction("printf");
        llvm::Constant *llStr = builder_->CreateGlobalStringPtr("[RUNTIME-ERROR]: array out-of-bounds access; offset by %d elements; size: %d\n", ".aobabrt_str");
        builder_->CreateCall(llPrintf, {llStr,
                                        llOffset, llArraySize});

        llvm::Function *llAbort = module_->getFunction("abort");
        builder_->CreateCall(llAbort);
        builder_->CreateUnreachable();
    }
}

void Codegen::genGlobalVars()
{
    globalScope_ = true;
    for (auto &u : ast_->getRoot()->declMap)
    {
        if (u.second->isRoutine)
            continue;
        u.second->codegen(*this);
    }

    globalScope_ = false;
}

void Codegen::genRoutines()
{
    for (auto &u : ast_->getRoot()->units)
    {
        auto *routine = dynamic_cast<Routine *>(u.get());
        if (routine)
            routine->codegen(*this);
    }
}

void Codegen::dump()
{
    module_->print(llvm::outs(), nullptr);
}

llvm::Constant *Codegen::getConstInitializer(const ast::Expr &node)
{
    if (!node.knownPrimitive)
        llvm_unreachable("getConstInitializer called on compile-time unknown expr");

    switch (node.code)
    {
    case ExprEnum::BoolLiteral:
        return ConstantInt::get(
            llvm::Type::getInt1Ty(*context_),
            static_cast<const BoolLiteral &>(node).val);
    case ExprEnum::IntLiteral:
        return ConstantInt::get(
            globalTys_.integer,
            static_cast<const IntLiteral &>(node).val);
    case ExprEnum::RealLiteral:
        return ConstantFP::get(
            globalTys_.real,
            static_cast<const RealLiteral &>(node).val);
    }
    llvm_unreachable("Codegen getConstInitializer: unexpected code");
}

llvm::FunctionType *Codegen::genRoutineType(const ast::RoutineType &node)
{
    std::vector<llvm::Type *> llParamTy;
    llParamTy.reserve(node.params.size());

    for (auto &param : node.params)
    {
        std::cerr << "get param ty\n";
        llvm::Type *llTy = getType(*param->type);
        std::cerr << "GOT?\n";
        if (!analyzer::isPrimitiveType(analyzer::getPureType(*param->type)))
            llTy = llTy->getPointerTo();
        std::cerr << "param type\n";
        llParamTy.push_back(llTy);
    }
    std::cerr << "gen ret yt\n";
    llvm::Type *llRetTy;
    if (node.retType)
    {
        llRetTy = getType(*node.retType);
        if (!analyzer::isPrimitiveType(analyzer::getPureType(*node.retType)))
        {
            llRetTy = llRetTy->getPointerTo();
        }
    }
    else
    {
        llRetTy = llvm::Type::getVoidTy(*context_);
    }
    std::cerr << "gened routine type\n";
    return llvm::FunctionType::get(llRetTy, llParamTy, false /* isVarArgs */);
}

void Codegen::convertToDouble(llvm::Value *&llVal)
{
    if (llVal->getType()->isIntegerTy())
        llVal = builder_->CreateSIToFP(llVal, globalTys_.real, "itof");
}

void Codegen::codegenBlock(const ast::Block &node, bool isFunctionEntry)
{
    llvm::Function *llParentFn = builder_->GetInsertBlock()->getParent();
    // FunctionEntry pushes the new stack into blockStack itself in the gen method for Routine
    if (!isFunctionEntry)
    {
        // Add to the block stack for accounting creation of heap objects
        blockStack_.emplace_back(std::list<HeapObj>{}, false);
        std::cerr << "push new BLOCKL\n";
    }
    BlockInfo &blockInfo = blockStack_.back();
    std::cerr << "new Block " << blockStack_.size() << " : " << blockInfo.heapObjs.size() << "\n";
    for (auto &u : node.units)
    {
        std::cerr << "unit " << u->span.line << " " << u->span.start << " " << u->span.end << "\n";
        u->codegen(*this);
        std::cerr << "codegeneed\n";
        // Destroying temporarily-allocated heap objects after a statement has ended
        if (!tmpHeapObjects_.empty())
        {
            BasicBlock *llTmpDestructorBlk = BasicBlock::Create(*context_, "tmp_destructor", llParentFn);
            BasicBlock *llBackBlk = BasicBlock::Create(*context_, "back", llParentFn);
            builder_->CreateBr(llTmpDestructorBlk);

            builder_->SetInsertPoint(llTmpDestructorBlk);

            if (config_.printHeapManagement)
            {
                builder_->CreateCall(module_->getFunction("printf"), builder_->CreateGlobalStringPtr("HEAP: calling tmp object destructor...\n"));
            }

            for (auto &heapObj : tmpHeapObjects_)
            {
                std::cerr << "decr tmp heap obj\n";
                heapObjUseCountDecr(heapObj.llPtr, heapObj.type);

                // rm from heapObjs, since the count is already decremented right after the statement
                auto it = std::find_if(blockInfo.heapObjs.begin(), blockInfo.heapObjs.end(),
                                       [&heapObj](HeapObj &obj)
                                       { return obj.llPtr == heapObj.llPtr; });
                if (it != blockInfo.heapObjs.end())
                    blockInfo.heapObjs.erase(it);
                std::cerr << "decr done\n";
            }

            builder_->CreateBr(llBackBlk);
            builder_->SetInsertPoint(llBackBlk);
            tmpHeapObjects_.clear();
        }
    }
    std::cerr << blockInfo.heapObjs.size() << " " << blockInfo.heapObjs.empty() << " done with units\n";
    if (!blockInfo.heapObjs.empty())
    {
        llvm::Instruction *last = !builder_->GetInsertBlock()->empty()
                                      ? &builder_->GetInsertBlock()->back()
                                      : nullptr;
        if (!last || !llvm::isa<llvm::ReturnInst>(last))
        {
            std::cerr << "closing blk\n";
            BasicBlock *llClosingBlk = BasicBlock::Create(*context_, "destructor", llParentFn);
            builder_->CreateBr(llClosingBlk);
            std::cerr << "ne1\n";
            builder_->SetInsertPoint(llClosingBlk);
            std::cerr << blockInfo.heapObjs.size() << " size n2\n";

            if (config_.printHeapManagement)
            {
                builder_->CreateCall(module_->getFunction("printf"), builder_->CreateGlobalStringPtr("HEAP: calling block destructor...\n"));
            }

            // Call decrement on all heap objects created in this block
            for (auto &heapObj : blockInfo.heapObjs)
            {
                std::cerr << heapObj.llPtr << " heap ptr\n";
                llvm::Value *llDeref = builder_->CreateLoad(globalTys_.heapObj->getPointerTo(), heapObj.llPtr);
                std::cerr << "n4\n";
                heapObjUseCountDecr(llDeref, heapObj.type);
            }
            std::cerr << "n3\n";
            BasicBlock *llBackBlk = BasicBlock::Create(*context_, "back", llParentFn);
            builder_->CreateBr(llBackBlk);
            std::cerr << "n4\n";
            builder_->SetInsertPoint(llBackBlk);
        }
    }
    std::cerr << "Done with Block\n";
    blockStack_.pop_back();
}

llvm::Value *Codegen::newHeapObject(const ast::Type &t, llvm::Type *llTy, llvm::IRBuilder<> &builder)
{
    const ast::Type &type = analyzer::getPureType(t);

    if (!llTy->isStructTy())
        llvm_unreachable("newHeapObject: unexpected llTy");

    if (config_.printHeapManagement)
    {
        builder.CreateCall(module_->getFunction("printf"), builder.CreateGlobalStringPtr("***HEAP: creating new object\n"));
    }

    llvm::StructType *llStructTy = static_cast<llvm::StructType *>(llTy);

    const llvm::DataLayout &llDl = module_->getDataLayout();
    uint64_t sizeInBytes = llDl.getTypeAllocSize(llTy);

    llvm::Type *ptrSizeTy = module_->getDataLayout().getIntPtrType(*context_, /*AddressSpace=*/0);
    llvm::Function *llMalloc = module_->getFunction("malloc");
    llvm::Value *llSize = llvm::ConstantInt::get(ptrSizeTy, sizeInBytes);
    llvm::Value *llPtr = builder.CreateCall(llMalloc, {llSize}, "obj");

    // Initialize heap-object fields
    const auto elemNum = llStructTy->getNumElements();
    std::vector<llvm::Constant *> llInitFields;
    llInitFields.reserve(elemNum);

    // ref_count init to 1
    llInitFields.push_back(llvm::ConstantInt::get(globalTys_.refcSize, 1));

    unsigned rest_offset;
    llvm::ArrayType *llArrayTy;

    if (type.code == TypeEnum::Array)
    {
        rest_offset = 2;

        auto it = typeArrayHashMap_.find(llTy);
        if (it == typeArrayHashMap_.end())
            llvm_unreachable("newHeapObject: no array type found in map");

        // fixed size init
        llArrayTy = it->second;
        llInitFields.push_back(llvm::ConstantInt::get(globalTys_.arraySize, llArrayTy->getNumElements()));
    }
    else
    {
        rest_offset = 1;
    }
    // Initialize the rest
    for (unsigned i = rest_offset; i < elemNum; ++i)
    {
        llvm::Type *llElemTy = llStructTy->getElementType(i);
        llInitFields.push_back(llvm::Constant::getNullValue(llElemTy));
    }

    llvm::Value *nullInit = llvm::ConstantStruct::get(llStructTy, llInitFields);
    builder.CreateStore(nullInit, llPtr);

    // For complex types, recursively heap-allocate and initialize the parent's field with the ptr
    if (type.code == TypeEnum::Record)
    {
        for (unsigned i = 1; i < elemNum; ++i)
        {
            const ast::Type &elemType = analyzer::getPureType(*static_cast<const RecordType &>(type).members[i - 1]->type);
            if (!analyzer::isPrimitiveType(elemType))
            {
                llvm::Type *llFieldTy = getType(elemType);
                llvm::Value *llFieldHeapPtr = newHeapObject(elemType, llFieldTy, builder);
                llvm::Value *llFieldPtr = builder.CreateStructGEP(llStructTy, llPtr, i, "field_obj");
                builder.CreateStore(llFieldHeapPtr, llFieldPtr);
            }
        }
    }
    else if (type.code == TypeEnum::Array)
    {
        const ast::Type &elemType = analyzer::getPureType(*static_cast<const ast::ArrayType &>(type).elemType);
        if (!analyzer::isPrimitiveType(elemType))
        {
            // Allocate all elements immediately in a while loop
            llvm::Value *llArrayBasePtr = builder.CreateStructGEP(llStructTy, llPtr, 2);

            llvm::Function *llParentFn = builder.GetInsertBlock()->getParent();
            llvm::BasicBlock *llLoopBlk = llvm::BasicBlock::Create(*context_, "alloc_loop", llParentFn);
            llvm::BasicBlock *llLoopBodyBlk = llvm::BasicBlock::Create(*context_, "alloc_loopbody", llParentFn);
            llvm::BasicBlock *llBrkBlk = llvm::BasicBlock::Create(*context_, "alloc_loopbrk", llParentFn);

            llvm::Value *llCounterVar = builder.CreateAlloca(globalTys_.arraySize, nullptr, "arr_it");
            builder.CreateStore(llvm::ConstantInt::get(globalTys_.arraySize, 0), llCounterVar);
            builder.CreateBr(llLoopBlk);

            builder.SetInsertPoint(llLoopBlk);
            llvm::Value *llCounterValue = builder.CreateLoad(globalTys_.arraySize, llCounterVar);
            llvm::Value *llCond = builder.CreateICmpSLT(llCounterValue, llInitFields[1]);
            builder.CreateCondBr(llCond, llLoopBodyBlk, llBrkBlk);

            builder.SetInsertPoint(llLoopBodyBlk);
            llvm::Type *llElemTy = getType(elemType);
            llvm::Value *llObjHeapPtr = newHeapObject(elemType, llElemTy, builder);
            llCounterValue = builder.CreateLoad(globalTys_.arraySize, llCounterVar);
            llvm::Value *llElemPtr = builder.CreateGEP(llArrayTy, llArrayBasePtr,
                                                       {
                                                           llvm::ConstantInt::get(globalTys_.arraySize, 0),
                                                           llCounterValue,
                                                       },
                                                       "elem_obj");
            builder.CreateStore(llObjHeapPtr, llElemPtr);

            llvm::Value *llStep = builder.CreateAdd(llCounterValue, llvm::ConstantInt::get(globalTys_.arraySize, 1));
            builder.CreateStore(llStep, llCounterVar);
            builder.CreateBr(llLoopBlk);

            builder.SetInsertPoint(llBrkBlk);
        }
    }
    else
        llvm_unreachable("newHeapObject: unexpected type");

    std::cerr << "NEW OBJ " << llPtr << "\n";

    return llPtr;
}

void Codegen::heapObjUseCountInc(llvm::Value *llPtr)
{
    builder_->CreateCall(globals_.refc_inc, {llPtr});
}

void Codegen::heapObjUseCountDecr(llvm::Value *llPtr, const ast::Type &t)
{
    const ast::Type &type = analyzer::getPureType(t);
    bool hasObjMembers = false;

    if (type.code == TypeEnum::Record)
    {
        const auto &members = static_cast<const RecordType &>(type).members;
        for (size_t i = 0; i < members.size(); ++i)
        {
            const auto &memberType = analyzer::getPureType(*members[i]->type);
            if (!analyzer::isPrimitiveType(memberType))
            {
                hasObjMembers = true;
                break;
            }
        }
    }
    else if (type.code == TypeEnum::Array)
    {
        const ast::Type &elemType = analyzer::getPureType(*static_cast<const ast::ArrayType &>(type).elemType);
        if (!analyzer::isPrimitiveType(elemType))
            hasObjMembers = true;
    }
    else
        llvm_unreachable("heapObjUseCountDecr: unexpected provided type " + std::to_string(static_cast<int>(type.code)));

    llvm::Value *llMustBeFreed = builder_->CreateCall(globals_.refc_dcr, {llPtr});

    llvm::Function *llParentFn = builder_->GetInsertBlock()->getParent();
    llvm::BasicBlock *llFreeBlk = llvm::BasicBlock::Create(*context_, "d_free", llParentFn);
    llvm::BasicBlock *llContBlk = llvm::BasicBlock::Create(*context_, "d_cont", llParentFn);
    builder_->CreateCondBr(llMustBeFreed, llFreeBlk, llContBlk);
    builder_->SetInsertPoint(llFreeBlk);

    if (!hasObjMembers)
    {
        // this heap object has only primitive members, just free it

        heapObjDestroy(llPtr);
        builder_->CreateBr(llContBlk);
        builder_->SetInsertPoint(llContBlk);
        return;
    }

    // this heap object contains other heap objects inside
    // decrement their refcount before freeing

    if (type.code == TypeEnum::Record)
    {
        const auto &members = static_cast<const RecordType &>(type).members;
        for (size_t i = 0; i < members.size(); ++i)
        {
            const auto &memberType = analyzer::getPureType(*members[i]->type);
            if (!analyzer::isPrimitiveType(memberType))
            {
                llvm::Type *llTy = getType(type);
                llvm::Value *llMemberPtr = builder_->CreateStructGEP(llTy, llPtr, i + 1, "field_ptr");
                llvm::Value *llDeref = builder_->CreateLoad(globalTys_.heapObj->getPointerTo(), llMemberPtr);
                heapObjUseCountDecr(llDeref, memberType);
            }
        }
    }
    else
    {
        const ast::Type &elemType = analyzer::getPureType(*static_cast<const ast::ArrayType &>(type).elemType);

        // decrement on each element in a while loop
        llvm::Type *llStructTy = getType(type);
        auto it = typeArrayHashMap_.find(llStructTy);
        if (it == typeArrayHashMap_.end())
            llvm_unreachable("heapObjUseCountDecr: array type not found in map");
        llvm::ArrayType *llArrayTy = it->second;
        llvm::Value *llSizeRttiPtr = builder_->CreateStructGEP(globalTys_.heapArrayObj, llPtr, 1);
        llvm::Value *llArraySize = builder_->CreateLoad(globalTys_.arraySize, llSizeRttiPtr);
        llvm::Value *llArrayBasePtr = builder_->CreateStructGEP(llStructTy, llPtr, 2);

        llvm::Function *llParentFn = builder_->GetInsertBlock()->getParent();
        llvm::BasicBlock *llLoopBlk = llvm::BasicBlock::Create(*context_, "refc_dcr_loop", llParentFn);
        llvm::BasicBlock *llLoopBodyBlk = llvm::BasicBlock::Create(*context_, "refc_dcr_loopbody", llParentFn);
        llvm::BasicBlock *llBrkBlk = llvm::BasicBlock::Create(*context_, "refc_dcr_loopbrk", llParentFn);

        llvm::Value *llCounterVar = builder_->CreateAlloca(globalTys_.arraySize, nullptr, "arr_it");
        builder_->CreateStore(llvm::ConstantInt::get(globalTys_.arraySize, 0), llCounterVar);
        builder_->CreateBr(llLoopBlk);

        builder_->SetInsertPoint(llLoopBlk);
        llvm::Value *llCounterValue = builder_->CreateLoad(globalTys_.arraySize, llCounterVar);
        llvm::Value *llCond = builder_->CreateICmpSLT(llCounterValue, llArraySize);
        builder_->CreateCondBr(llCond, llLoopBodyBlk, llBrkBlk);

        builder_->SetInsertPoint(llLoopBodyBlk);
        llCounterValue = builder_->CreateLoad(globalTys_.arraySize, llCounterVar);
        llvm::Value *llElemPtr = builder_->CreateGEP(llArrayTy, llArrayBasePtr,
                                                     {
                                                         llvm::ConstantInt::get(globalTys_.arraySize, 0),
                                                         llCounterValue,
                                                     },
                                                     "elem_obj");
        llvm::Value *llElem = builder_->CreateLoad(globalTys_.heapObj->getPointerTo(), llElemPtr);
        heapObjUseCountDecr(llElem, elemType);
        llvm::Value *llStep = builder_->CreateAdd(llCounterValue, llvm::ConstantInt::get(globalTys_.arraySize, 1));
        builder_->CreateStore(llStep, llCounterVar);
        builder_->CreateBr(llLoopBlk);

        builder_->SetInsertPoint(llBrkBlk);
    }

    heapObjDestroy(llPtr);
    builder_->CreateBr(llContBlk);
    builder_->SetInsertPoint(llContBlk);
}

void Codegen::heapObjDestroy(llvm::Value *llPtr)
{
    llvm::Function *llFreeFn = module_->getFunction("free");
    builder_->CreateCall(llFreeFn, {llPtr});

    if (config_.printHeapManagement)
    {
        builder_->CreateCall(module_->getFunction("printf"), builder_->CreateGlobalStringPtr("***HEAP: object freed\n"));
    }
}

llvm::Value *Codegen::codegenPrimaryPtr(const ast::Entity &node)
{
    getVarPtr_ = true;
    llvm::Value *llPtr = node.codegen(*this);
    getVarPtr_ = false;

    if (!llPtr->getType()->isPointerTy())
        llvm_unreachable("codegenIdRefPtr: codegen did not return a Value of ptr type");

    return llPtr;
}