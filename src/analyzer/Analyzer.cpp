#include "parser/Parser.h"
#include "analyzer/Analyzer.h"
#include "analyzer/utils.h"

using namespace analyzer;
using namespace Ast;

int Analyzer::configure(int* argc, char** argv) {
    return 0;
}

void Analyzer::run() {
    root_ = parser_.getRoot();
    currBlock_ = root_.get();
    validate(*currBlock_);

    for (auto& routine: undefinedRoutines_) {
        saveError("routine " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
            + routine.second->id + ANSI_RESET
            + " has not been defined",
            routine.second->span
        );
    }

    /* Validating main routine*/ {
        auto it = root_->declMap.find("main");
        if (it == root_->declMap.end()) {
            saveError("no 'main' routine defined", Tokens::Span{1,0,1});
            return;
        }
        if (!it->second->isRoutine) {
            saveError("'main' must be a routine", it->second->span);
            return;
        }
        if (std::static_pointer_cast<RoutineType>(it->second->type)->retType) {
            saveError("'main' routine cannot return anything", it->second->span);
            return;
        }
        if (!std::static_pointer_cast<RoutineType>(it->second->type)->params.empty()) {
            saveError("'main' routine cannot have parameters", it->second->span);
            return;
        }
        it->second->everUsed = true;
    }

    optimizer_.onBlockFinish(*root_);
}

void Analyzer::saveError(std::string reason, Tokens::Span span) {
    reporter_.report({
        .level = CompileMsg::Level::Error,
        .message = std::move(reason),
        .span = span,
    });
}

std::shared_ptr<Decl> Analyzer::searchDeclaration(const std::string& id) {
    auto* block = currBlock_;
    while (block) {
        auto it = block->declMap.find(id);
        if (it != block->declMap.end())
            return it->second;

        auto lock = block->parent.lock();
        if (!lock)
            break;
        block = lock.get();
    }
    return nullptr;
}

void Analyzer::invalidateKnownVarsInCurrBlock() {
    for (auto& it: currBlock_->declMap) {
        if (it.second->isRoutine) continue;
        std::static_pointer_cast<Var>(it.second)->knownPrimitive = false;
    }
}

void Analyzer::invalidateKnownVarByRef(IdRef& node) {
    auto it = currBlock_->declMap.find(node.id);
    if (it == currBlock_->declMap.end() || it->second->isRoutine)
        return;
    std::static_pointer_cast<Var>(it->second)->knownPrimitive = false;
}

void Analyzer::validate(TypeRef& node) {
    auto* block = currBlock_;
    while (block) {
        auto it = block->typeMap.find(node.id);
        if (it != block->typeMap.end()) {
            node.ref = it->second;
            return;
        }

        auto lock = block->parent.lock();
        if (!lock)
            break;
        block = lock.get();
    }

    saveError(
        "use of undeclared type: " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + node.id + ANSI_RESET,
        node.span
    );
}

void Analyzer::validate(TypeDecl& node) {
    node.type->validate(*this);
    currBlock_->typeMap.emplace(node.id, std::static_pointer_cast<TypeDecl>(node.shared_from_this()));
}

void Analyzer::validate(Block& node) {
    invalidateKnownVarsInCurrBlock();
    currBlock_ = &node;

    for (auto& unit: node.units) {
        if (deadCode_)
            saveError("statement after return will never be executed", unit->span);
        unit->validate(*this);
    }

    if (currBlock_ && currBlock_ != root_.get())
        optimizer_.onBlockFinish(*currBlock_);

    currBlock_ = currBlock_->parent.lock().get();
    deadCode_ = false;
}

void Analyzer::validate(IntRange& node) {
    node.start->validate(*this);
    if (node.start->type->code != TypeEnum::Int) {
        saveError("start of iteration range is not of integer type; actual: "
            + stringifyType(node.start->type),
            node.start->span
        );
    } else {
        auto opt = optimizer_.computeExpr(*node.start);
        if (opt) node.start = std::move(opt);
    }

    node.end->validate(*this);
    if (node.end->type->code != TypeEnum::Int) {
        saveError("end of iteration range is not of integer type; actual: "
            + stringifyType(node.end->type),
            node.end->span
        );
    } else {
        auto opt = optimizer_.computeExpr(*node.end);
        if (opt) node.end = std::move(opt);
    }
}

void Analyzer::validate(ArrayIdRange& node) {
    auto decl = searchDeclaration(node.id);
    if (!decl) {
        saveError(
            "use of undeclared identifier " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + node.id + ANSI_RESET,
            node.span
        );
        return;
    }

    if (decl->type->code != TypeEnum::Array) {
        saveError("object " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + node.id
            + ANSI_RESET + " is not of array type; actual: " + stringifyType(decl->type),
            node.span
        );
    }

    node.ref = std::static_pointer_cast<Var>(decl);
}

void Analyzer::validate(IdRef& node) {
    if (!idRef_.head) {
        auto decl = searchDeclaration(node.id);
        if (!decl) {
            node.type = parser_.getBaseTypes().error;
            saveError(
                "use of undeclared identifier: " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + node.id + ANSI_RESET,
                node.span
            );
            return;
        }

        decl->everUsed = true;
        node.ref = decl;
        idRef_.head = &node;
        idRef_.currType = &decl->type;

        if (node.next) {
            idRef_.prev = &node;
            node.next->validate(*this);
        }
        node.type = *idRef_.currType;
        idRef_.head = nullptr;
    } else {
        if ((*idRef_.currType)->code != TypeEnum::Record) {
            idRef_.currType = &parser_.getBaseTypes().error;
            saveError(
                "object " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
                + file_->extractSrc(idRef_.head->span.start, node.span.end) + ANSI_RESET
                + " is not of record type; actual type: "
                + ANSI_START ANSI_BOLD ANSI_APPLY + stringifyType(*idRef_.currType) + ANSI_RESET,
                node.span
            );
            return;
        }

        auto it = std::find_if(
            static_cast<RecordType&>(**idRef_.currType).members.begin(),
            static_cast<RecordType&>(**idRef_.currType).members.end(),
            [&node](std::shared_ptr<Var> var) { return var->id == node.id; }
        );

        if (it == static_cast<RecordType&>(**idRef_.currType).members.end()) {
            idRef_.currType = &parser_.getBaseTypes().error;
            saveError(
                "object " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
                + file_->extractSrc(idRef_.head->span.start, idRef_.prev->span.end) + ANSI_RESET
                + " of type " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(*idRef_.currType) + ANSI_RESET + " does not have a field '"
                + ANSI_START ANSI_BOLD ANSI_APPLY + node.id + ANSI_RESET + "'",
                node.span
            );
            return;
        }

        idRef_.currType = &(*it)->type;

        if (node.next) {
            idRef_.prev = &node;
            node.next->validate(*this);
        }
    }
}

void Analyzer::validate(BinaryExpr& node) {
    auto& expr = static_cast<BinaryExpr&>(node);
    expr.left->validate(*this);
    expr.right->validate(*this);
    
    if (!expr.left->type || expr.left->type->code == TypeEnum::ERROR
        || !expr.right->type || expr.right->type->code == TypeEnum::ERROR)
    {
        expr.type = parser_.getBaseTypes().error;
        return;
    }

    switch (node.code) {
    case ExprEnum::Add:
    case ExprEnum::Subtract:
    case ExprEnum::Multiply:
    case ExprEnum::Divide: {
        if ((expr.left->type->code != TypeEnum::Int && expr.left->type->code != TypeEnum::Real)
            || (expr.right->type->code != TypeEnum::Int && expr.right->type->code != TypeEnum::Real)
        ) {
            expr.type = parser_.getBaseTypes().error;
            saveError(
                "binary operator used with incompatible types: " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
                + stringifyType(expr.left->type)
                + ANSI_RESET + " and " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(expr.right->type) + ANSI_RESET,
                expr.span
            );
            return;
        }

        if (expr.left->type->code == TypeEnum::Real || expr.right->type->code == TypeEnum::Real)
            expr.type = parser_.getBaseTypes().real;
        else
            expr.type = parser_.getBaseTypes().integer;
        break;
    }
    case ExprEnum::Modulo: {
        if (expr.left->type->code != TypeEnum::Int || expr.right->type->code != TypeEnum::Int) {
            expr.type = parser_.getBaseTypes().error;
            saveError(
                "modulo operator can only be applied to integers, but got: " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
                + stringifyType(expr.left->type)
                + ANSI_RESET + " and " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(expr.right->type) + ANSI_RESET,
                expr.span
            );
            return;
        }

        if (expr.left->type->code == TypeEnum::Real || expr.right->type->code == TypeEnum::Real)
            expr.type = parser_.getBaseTypes().real;
        else
            expr.type = parser_.getBaseTypes().integer;
        break;
    }
    case ExprEnum::And:
    case ExprEnum::Or:
    case ExprEnum::Xor: {
        if (expr.left->type->code != TypeEnum::Bool || expr.right->type->code != TypeEnum::Bool) {
            std::string op = (expr.code == ExprEnum::And ? "and"
                : (expr.code == ExprEnum::Or ? "or" : "xor"));
            saveError(
                "arguments of operator '" + op + "' are not of boolean type;\n\tleft side: "
                + ANSI_START ANSI_BOLD ANSI_APPLY + stringifyType(expr.left->type)
                + ANSI_RESET + "\n\tright side: " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(expr.right->type) + ANSI_RESET,
                expr.span
            );
        }

        expr.type = parser_.getBaseTypes().boolean;
        break;
    }
    case ExprEnum::LESS_THAN:
    case ExprEnum::LESS_OR_EQUAL:
    case ExprEnum::MORE_THAN:
    case ExprEnum::MORE_OR_EQUAL: {
        if ((expr.left->type->code != TypeEnum::Int && expr.left->type->code != TypeEnum::Real)
            || (expr.right->type->code != TypeEnum::Int && expr.right->type->code != TypeEnum::Real)
        ) {
            saveError(
                "binary operator used with incompatible types: " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
                + stringifyType(expr.left->type)
                + ANSI_RESET + " and " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(expr.right->type) + ANSI_RESET,
                expr.span
            );
        }

        expr.type = parser_.getBaseTypes().boolean;
        break;
    }
    case ExprEnum::EQUAL:
    case ExprEnum::UNEQUAL: {
        if (
            !isErrorType(expr.left->type) && !isErrorType(expr.right->type) && (
                !areTypesEqual(expr.left->type, expr.right->type)
                || !isPrimitiveType(expr.left->type) || !isPrimitiveType(expr.right->type)
            )
        ) {
            saveError("equality used with incompatible types: " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
                + stringifyType(expr.left->type)
                + ANSI_RESET + " and " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(expr.right->type) + ANSI_RESET,
                expr.span
            );
        }

        expr.type = parser_.getBaseTypes().boolean;
        break;
    }
    }
}

void Analyzer::validate(UnaryExpr& node) {
    node.val->validate(*this);

    switch (node.code) {
    case ExprEnum::Negate: {
        if (node.val->type->code != TypeEnum::Int && node.val->type->code != TypeEnum::Real) {
            node.type = parser_.getBaseTypes().error;
            saveError(
                "unary minus applied to non-numeric type; actual: "
                + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
                + stringifyType(node.val->type) + ANSI_RESET,
                node.val->span
            );
            return;
        }
        node.type = node.val->type;
        
        break;
    }
    case ExprEnum::Not: {
        if (node.val->type->code != TypeEnum::Bool) {
            node.type = parser_.getBaseTypes().error;
            saveError(
                "'not' applied to non-boolean type; actual: "
                + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
                + stringifyType(node.val->type) + ANSI_RESET,
                node.val->span
            );
            return;
        }
        node.type = parser_.getBaseTypes().boolean;

        break;
    }
    default:
        throw std::runtime_error("invalid UnaryExpr.code " + std::to_string(static_cast<int>(node.code)));
    }
}

void Analyzer::validate(PrintStmt& node) {
    if (!currRoutine_) {
        saveError("print statement in global scope is illegal", node.span);
        return;
    }

    for (auto& arg: node.args) {
        arg->validate(*this);
        auto opt = optimizer_.computeExpr(*arg);
        if (opt) arg = std::move(opt);
    }
}

void Analyzer::validate(IfStmt& node) {
    if (!currRoutine_) {
        saveError("if statement in global scope is illegal", node.span);
        return;
    }

    node.condition->validate(*this);
    if (!isErrorType(node.condition->type) && node.condition->type->code != TypeEnum::Bool) {
        saveError("if condition must be of boolean type; actual "
            + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + stringifyType(node.condition->type) + ANSI_RESET,
            node.condition->span
        );
    }
    auto opt = optimizer_.computeExpr(*node.condition);
    if (opt) node.condition = std::move(opt);

    node.body->validate(*this);
    if (node.elseBody)
        node.elseBody->validate(*this);
}

void Analyzer::validate(WhileStmt& node) {
    if (!currRoutine_) {
        saveError("while statement in global scope is illegal", node.span);
        return;
    }

    node.condition->validate(*this);
    if (!isErrorType(node.condition->type) && node.condition->type->code != TypeEnum::Bool) {
        saveError("while condition must be of boolean type; actual "
            + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + stringifyType(node.condition->type) + ANSI_RESET,
            node.condition->span
        );
    }
    auto opt = optimizer_.computeExpr(*node.condition);
    if (opt) node.condition = std::move(opt);

    node.body->validate(*this);
}

void Analyzer::validate(ForStmt& node) {
    if (!currRoutine_) {
        saveError("for statement in global scope is illegal", node.span);
        return;
    }

    node.body->declMap.emplace(node.counter->id, node.counter);
    node.counter->type = parser_.getBaseTypes().integer;
    node.range->validate(*this);
    node.body->validate(*this);
}

void Analyzer::validate(ReturnStmt& node) {
    if (!currRoutine_) {
        saveError("return statement in global scope is illegal", node.span);
        return;
    }

    deadCode_ = true;

    if (node.val) {
        node.val->validate(*this);

        if (!currRoutine_->getType()->retType) {
            saveError(
                "in routine " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + currRoutine_->id
                + ANSI_RESET + ": return value specified, but routine does not return anything",
                node.span
            );
            return;
        }

        if (
            !isErrorType(currRoutine_->getType()->retType) && !isErrorType(node.val->type)
            && !areTypesEqual(node.val->type, currRoutine_->getType()->retType)
        ) {
            saveError(
                "in routine " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + currRoutine_->id
                + ANSI_RESET + ": return value type mismatch\n\texpected: "
                + ANSI_START ANSI_BOLD ANSI_APPLY + stringifyType(currRoutine_->getType()->retType)
                + ANSI_RESET + "\n\tbut got: " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(node.val->type) + ANSI_RESET,
                node.span
            );
        }

        auto opt = optimizer_.computeExpr(*node.val);
        if (opt) node.val = std::move(opt);
    } else {
        if (currRoutine_->getType()->retType) {
            saveError(
                "in routine " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + currRoutine_->id
                + ANSI_RESET + ": no value returned, but return type is " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(currRoutine_->getType()->retType) + ANSI_RESET,
                node.span
            );
        }
    }
}

void Analyzer::validate(Assignment& node) {
    node.left->validate(*this);
    if (node.left->type->code == TypeEnum::Routine) {
        saveError("cannot assign to routine identifier", node.left->span);
        return;
    }

    node.val->validate(*this);
    if (node.val->type->code == TypeEnum::ERROR) {
        invalidateKnownVarByRef(static_cast<IdRef&>(*node.left));
        return;
    }

    bool typesEqual = areTypesEqual(node.val->type, node.left->type);
    if (!typesEqual) {
        if (node.left->type->code == TypeEnum::Array || node.left->type->code == TypeEnum::Record) {
            invalidateKnownVarByRef(static_cast<IdRef&>(*node.left));
            saveError("assignment type mismatch\n\texpected: "
                + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + stringifyType(node.left->type)
                + ANSI_RESET + "\n\tbut got: "
                + ANSI_START ANSI_BOLD ANSI_APPLY + stringifyType(node.val->type) + ANSI_RESET,
                node.val->span
            );
            return;
        }

        if (node.left->type->code == TypeEnum::Bool) {
            if (node.val->type->code == TypeEnum::Real) {
                invalidateKnownVarByRef(static_cast<IdRef&>(*node.left));
                saveError("illegal assignment of real to boolean", node.val->span);
                return;
            }
        }
    }

    auto opt = optimizer_.computeExpr(*node.val);
    if (opt) node.val = std::move(opt);

    if (!typesEqual && node.left->type->code == TypeEnum::Bool
        && node.val->type->code == TypeEnum::Int)
    {
        if (!node.val->knownPrimitive
            || (std::static_pointer_cast<IntLiteral>(node.val)->val != 0
            && std::static_pointer_cast<IntLiteral>(node.val)->val != 1)) {
            
            invalidateKnownVarByRef(static_cast<IdRef&>(*node.left));
            saveError(
                "assignment of integer to boolean is only allowed if the left side is evaluated at compile-time and is either 0 or 1",
                node.val->span
            );
            return;
        }
    }

    Optimizer::AssignmentOptStatus res = optimizer_.optimizeAssignmentAway(node);
    if (res == Optimizer::AssignmentOptStatus::Skip)
        invalidateKnownVarByRef(static_cast<IdRef&>(*node.left));
    else if (res == Optimizer::AssignmentOptStatus::Fail) {
        invalidateKnownVarByRef(static_cast<IdRef&>(*node.left));
        if (!currRoutine_) {
            saveError("non-compile-time evaluated assignment in global scope is illegal", node.span);
        }
    }
}

void Analyzer::validate(Var& node) {
    auto it = currBlock_->declMap.find(node.id);
    if (it == currBlock_->declMap.end())
        currBlock_->declMap.emplace(node.id, node.shared_from_this());
    else {
        saveError(
            "redefinition of identifier " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
            + node.id + ANSI_RESET,
            node.span
        );
        reporter_.report({
            .level = CompileMsg::Level::Appendix,
            .message = "previously defined here",
            .span = it->second->span
        });
    }

    if (node.type) {
        node.type->validate(*this);
        if (node.type->code == TypeEnum::REFERENCE) {
            auto lock = reinterpret_cast<TypeRef&>(*node.type).ref.lock();
            if (lock)
                node.type = lock->type;
            else
                node.type = parser_.getBaseTypes().error;
        }
    }
    if (node.val) {
        node.val->validate(*this);
        if (node.type && node.val->type && !areTypesEqual(node.type, node.val->type)) {
            saveError(
                "type mismatch in initialization of "
                + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + node.id + ANSI_RESET
                + "\n\texpected: " + ANSI_START ANSI_BOLD ANSI_APPLY + stringifyType(node.type)
                + ANSI_RESET + "\n\tbut got: "
                + ANSI_START ANSI_BOLD ANSI_APPLY + stringifyType(node.val->type) + ANSI_RESET,
                node.span
            );
            return;
        }

        auto opt = optimizer_.computeExpr(*node.val);
        if (opt) node.val = std::move(opt);

        node.type = node.val->type;
        node.knownPrimitive = node.val->knownPrimitive;
    } else if (!analyzingRoutineParams_) {
        // default initializer

        if (node.type->code == TypeEnum::Bool) {
            node.knownPrimitive = true;
            node.val = parser_.getDefaultInitializers().boolean;
        } else if (node.type->code == TypeEnum::Int) {
            node.knownPrimitive = true;
            node.val = parser_.getDefaultInitializers().integer;
        } else if (node.type->code == TypeEnum::Real) {
            node.knownPrimitive = true;
            node.val = parser_.getDefaultInitializers().real;
        }
    }
}

// Check that routine was declared in global scope
void Analyzer::validate(Routine& node) {
    if (currBlock_ != root_.get()) {
        saveError(
            "routine declarations are only allowed in global scope",
            node.span
        );
        return;
    }

    if (!node.body) {
        undefinedRoutines_.emplace(node.id, std::static_pointer_cast<Routine>(node.shared_from_this()));
        return;
    }

    undefinedRoutines_.erase(node.id);

    auto it = currBlock_->declMap.find(node.id);
    if (it == currBlock_->declMap.end())
        currBlock_->declMap.emplace(node.id, node.shared_from_this());
    else {
        saveError(
            "redefinition of identifier " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
            + node.id + ANSI_RESET,
            node.span
        );
        reporter_.report({
            .level = CompileMsg::Level::Appendix,
            .message = "previously defined here",
            .span = it->second->span
        });
    }

    // set currBlock so that params are added into its declMap, not the outer one
    invalidateKnownVarsInCurrBlock();
    currBlock_ = node.body.get();

    // parameters will automatically be set to uknown at compile time upon entering the body->validate()
    analyzingRoutineParams_ = true;
    for (auto& param: node.getType()->params) {
        param->validate(*this);
        if (isErrorType(param->type))
            saveError("routine parameter has <error> type", param->span);
    }

    analyzingRoutineParams_ = false;
    currRoutine_ = &node;
    node.body->validate(*this);
    currRoutine_ = nullptr;
}

void Analyzer::validate(RoutineCall& node) {
    if (!currRoutine_) {
        saveError("routine call in global scope is illegal", node.span);
        return;
    }

    invalidateKnownVarsInCurrBlock();

    auto& expr = static_cast<RoutineCall&>(node);
    auto decl = searchDeclaration(expr.routineId);
    if (!decl) {
        node.type = parser_.getBaseTypes().error;
        saveError("use of undeclared identifier: "
            + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + expr.routineId + ANSI_RESET,
            expr.span
        );
        return;
    }
    if (!decl->isRoutine) {
        node.type = parser_.getBaseTypes().error;
        saveError("attempt to call a variable that is not a routine " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
            + expr.routineId + ANSI_RESET,
            expr.span
        );
        return;
    }

    decl->everUsed = true;
    auto& routine = static_cast<Routine&>(*decl);

    if (routine.getType()->params.size() != expr.args.size()) {
        saveError("in routine call to " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + routine.id
            + ANSI_RESET + ": argument count mismatch; expected "
            + std::to_string(routine.getType()->params.size())
            + ", but given " + std::to_string(expr.args.size()),
            Tokens::Span{expr.span.line, expr.span.start, expr.span.start + expr.routineId.size()}
        );
    }
    for (size_t i = 0, num = std::min(routine.getType()->params.size(), expr.args.size()); i < num; ++i) {
        expr.args[i]->validate(*this);
        if (expr.args[i]->code == ExprEnum::IdRef)
            invalidateKnownVarByRef(static_cast<IdRef&>(*expr.args[i]));

        if (
            !isErrorType(routine.getType()->params[i]->type)
            && !isErrorType(expr.args[i]->type)
            && !areTypesEqual(routine.getType()->params[i]->type, expr.args[i]->type)
        ) {
            saveError(
                "in routine call to " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + routine.id
                + ANSI_RESET + ": argument #" + std::to_string(i) + " type mismatch\n\t"
                + "expected: " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(routine.getType()->params[i]->type) + ANSI_RESET
                + "\n\tbut got: " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(expr.args[i]->type) + ANSI_RESET,
                expr.args[i]->span
            );
        }

        auto opt = optimizer_.computeExpr(*expr.args[i]);
        if (opt) expr.args[i] = std::move(opt);
    }
    if (!routine.body) {
        saveError("use of routine with no body: " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
            + expr.routineId + ANSI_RESET,
            expr.span
        );
    }

    node.type = routine.getType()->retType;
    node.ref = std::static_pointer_cast<Routine>(decl);
}

void Analyzer::validate(ArrayType& node) {
    node.elemType->validate(*this);

    if (node.size) {
        node.size->validate(*this);
        if (node.size->type->code != TypeEnum::Int) {
            saveError("array type size specified is not of integer type; actual: "
                + std::string(ANSI_START ANSI_BLUE ANSI_APPLY) + stringifyType(node.size->type)
                + ANSI_RESET,
                node.size->span
            );
            return;
        }

        auto opt = optimizer_.computeExpr(*node.size);
        if (opt) node.size = std::move(opt);

        if (!node.size->knownPrimitive) {
            saveError("array type size specified does not evaluate at compile-time", node.size->span);
        }
    } else {
        if (!analyzingRoutineParams_) {
            saveError("array type size must be specified", node.span);
        }
    }
}

void Analyzer::validate(ArrayAccess& node) {
    if ((*idRef_.currType)->code != TypeEnum::Array) {
        idRef_.currType = &parser_.getBaseTypes().error;
        saveError(
            "type " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
            + stringifyType(*idRef_.currType) + ANSI_RESET + "is not an array",
            node.span
        );
        return;
    }

    {
        auto state = idRef_;
        idRef_.head = nullptr;
        node.val->validate(*this);
        idRef_ = state;
    }

    if (!isErrorType(node.val->type) && node.val->type->code != TypeEnum::Int) {
        saveError(
            "array index is not of integer type; actual: "
            + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + stringifyType(node.val->type) + ANSI_RESET,
            node.span
        );
    } else {
        auto opt = optimizer_.computeExpr(*node.val);
        if (opt) node.val = std::move(opt);

        auto size = std::static_pointer_cast<IntLiteral>(std::static_pointer_cast<ArrayType>(*idRef_.currType)->size);
        auto val = std::static_pointer_cast<IntLiteral>(node.val);
        if (val->knownPrimitive && size->knownPrimitive) {
            if (val->val < 0 || val->val >= size->val) {
                saveError(
                    "array index out of bounds;\n\tmax index: "
                    + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + std::to_string(size->val - 1)
                    + ANSI_RESET + "\n\tprovided: " + ANSI_START ANSI_BOLD ANSI_APPLY
                    + std::to_string(val->val) + ANSI_RESET,
                    node.span
                );
            }
        }
    }

    idRef_.currType = &std::static_pointer_cast<ArrayType>(*idRef_.currType)->elemType;

    if (node.next) {
        idRef_.prev = &node;
        node.next->validate(*this);
    }
}

void Analyzer::validate(RecordType& node) {
    for (auto& member: node.members) {
        if (member->type)
            member->type->validate(*this);
        if (member->val)
            saveError("record type members cannot have initializers", member->val->span);
    }
}