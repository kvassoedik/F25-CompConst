#include "analyzer/Analyzer.h"
#include "parser/Parser.h"
#include "utils/PrintingUtils.h"
#include <sstream>

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
            + "has not been defined",
            routine.second->span
        );
    }
}

void Analyzer::saveError(std::string reason, Tokens::Span span) {
    reporter_.report({
        .level = CompileMsg::Level::Error,
        .message = std::move(reason),
        .span = span,
    });
}

bool Analyzer::areTypesEqual(const std::shared_ptr<Ast::Type>& t1, const std::shared_ptr<Ast::Type>& t2) {
    if (t1->code != t2->code)
        return false;

    switch (t1->code) {
    case Ast::TypeEnum::Array: {
        const auto& at1 = static_cast<Ast::ArrayType&>(*t1), at2 = static_cast<Ast::ArrayType&>(*t2);
        if (at1.size && at2.size) {
            if (!at1.size->known || !at2.size->known
                || at1.size->code != Ast::ExprEnum::IntLiteral
                || at2.size->code != Ast::ExprEnum::IntLiteral
                || static_cast<Ast::IntLiteral&>(*at1.size).val != static_cast<Ast::IntLiteral&>(*at2.size).val
            )
                return false;
        }

        if (!at1.elemType
            || !at2.elemType
            || !areTypesEqual(at1.elemType, at2.elemType)
        )
            return false;
        break;
    }
    case Ast::TypeEnum::Record: {
        const auto& at1 = static_cast<Ast::RecordType&>(*t1), at2 = static_cast<Ast::RecordType&>(*t2);
        if (at1.members.size() != at2.members.size())
            return false;

        for (size_t i = 0; i < at1.members.size(); ++i) {
            if (!areTypesEqual(at1.members[i]->type, at2.members[i]->type))
                return false;
        }
        break;
    }
    }
    
    return true;
}

bool Analyzer::isErrorType(const std::shared_ptr<Ast::Type> type) const noexcept { return type->code == Ast::TypeEnum::ERROR; }

std::string Analyzer::stringifyType(const std::shared_ptr<Ast::Type>& t) {
    std::stringstream out;
    t->printType(printer_, {.os = out});
    return out.str();
}

std::shared_ptr<Ast::Decl> Analyzer::searchDeclaration(const std::string& id) {
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

void Analyzer::validate(Ast::TypeRef& node) {
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

void Analyzer::validate(Ast::TypeDecl& node) {
    node.type->validate(*this);
    currBlock_->typeMap.emplace(node.id, std::static_pointer_cast<Ast::TypeDecl>(node.shared_from_this()));
}

void Analyzer::validate(Ast::Block& node) {
    currBlock_ = &node;
    for (auto& unit: node.units) {
        unit->validate(*this);
    }

    currBlock_ = currBlock_->parent.lock().get();
}

void Analyzer::validate(Ast::IntRange& node) {
    if (node.start->type->code != Ast::TypeEnum::Int) {
        saveError("start of iteration range is not of integer type; actual: "
            + stringifyType(node.start->type),
            node.start->span
        );
    }
    if (node.end->type->code != Ast::TypeEnum::Int) {
        saveError("end of iteration range is not of integer type; actual: "
            + stringifyType(node.end->type),
            node.end->span
        );
    }
}

void Analyzer::validate(Ast::ArrayIdRange& node) {
    auto decl = searchDeclaration(node.id);
    if (!decl) {
        saveError(
            "use of undeclared identifier " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + node.id + ANSI_RESET,
            node.span
        );
        return;
    }

    if (decl->type->code != Ast::TypeEnum::Array) {
        saveError("object " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + node.id
            + ANSI_RESET + " is not of array type; actual: " + stringifyType(decl->type),
            node.span
        );
    }

    node.ref = std::static_pointer_cast<Ast::Var>(decl);
}

// Check that identifier was declared
void Analyzer::validate(Ast::IdRef& node) {
    if (!idRef_.head) {
        auto decl = searchDeclaration(node.id);
        if (!decl) {
            saveError(
                "use of undeclared identifier: " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + node.id + ANSI_RESET,
                node.span
            );
            return;
        }

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
        if ((*idRef_.currType)->code != Ast::TypeEnum::Record) {
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
            static_cast<Ast::RecordType&>(**idRef_.currType).members.begin(),
            static_cast<Ast::RecordType&>(**idRef_.currType).members.end(),
            [&node](std::shared_ptr<Ast::Var> var) { return var->id == node.id; }
        );

        if (it == static_cast<Ast::RecordType&>(**idRef_.currType).members.end()) {
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

void Analyzer::validate(Ast::BinaryExpr& node) {
    switch (node.code) {
    case Ast::ExprEnum::Add:
    case Ast::ExprEnum::Subtract:
    case Ast::ExprEnum::Multiply:
    case Ast::ExprEnum::Divide:
    case Ast::ExprEnum::Modulo:
    case Ast::ExprEnum::LESS_THAN:
    case Ast::ExprEnum::LESS_OR_EQUAL:
    case Ast::ExprEnum::MORE_THAN:
    case Ast::ExprEnum::MORE_OR_EQUAL: {
        auto& expr = static_cast<Ast::BinaryExpr&>(node);
        expr.left->validate(*this);
        expr.right->validate(*this);

        if (!expr.left->type || expr.left->type->code == Ast::TypeEnum::ERROR
            || !expr.right->type || expr.right->type->code == Ast::TypeEnum::ERROR)
        {
            expr.type = parser_.getBaseTypes().error;
            return;
        }

        if ((expr.left->type->code != Ast::TypeEnum::Int && expr.left->type->code != Ast::TypeEnum::Real)
            || (expr.right->type->code != Ast::TypeEnum::Int && expr.right->type->code != Ast::TypeEnum::Real)
        ) {
            saveError("binary operator used with incompatible types: " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
                + stringifyType(expr.left->type)
                + ANSI_RESET + "and " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(expr.right->type) + ANSI_RESET,
                expr.span
            );
        }

        if (expr.left->type->code == Ast::TypeEnum::Real || expr.right->type->code == Ast::TypeEnum::Real)
            expr.type = parser_.getBaseTypes().real;
        else
            expr.type = parser_.getBaseTypes().integer;

        if (expr.left->known && expr.right->known)
            expr.known = true;

        break;
    }
    case Ast::ExprEnum::And:
    case Ast::ExprEnum::Or:
    case Ast::ExprEnum::Xor: {
        auto& expr = static_cast<Ast::BinaryExpr&>(node);
        expr.left->validate(*this);
        expr.right->validate(*this);
        
        if (!expr.left->type || expr.left->type->code == Ast::TypeEnum::ERROR
            || !expr.right->type || expr.right->type->code == Ast::TypeEnum::ERROR)
        {
            expr.type = parser_.getBaseTypes().error;
            return;
        }

        // Attempt to cast to boolean
        if (expr.left->type->code != Ast::TypeEnum::Bool || expr.right->type->code != Ast::TypeEnum::Bool) {
            saveError("arguments of binary operator cannot be casted to boolean", expr.span);
            expr.type = parser_.getBaseTypes().error;
        }

        expr.type = parser_.getBaseTypes().boolean;

        break;
    }
    case Ast::ExprEnum::EQUAL:
    case Ast::ExprEnum::UNEQUAL: {
        auto& expr = static_cast<Ast::BinaryExpr&>(node);
        expr.left->validate(*this);
        expr.right->validate(*this);
        
        if (!expr.left->type || expr.left->type->code == Ast::TypeEnum::ERROR
            || !expr.right->type || expr.right->type->code == Ast::TypeEnum::ERROR)
        {
            expr.type = parser_.getBaseTypes().error;
            return;
        }

        if (!areTypesEqual(expr.left->type, expr.right->type)) {
            saveError("equality used with incompatible types: " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
                + stringifyType(expr.left->type)
                + ANSI_RESET + "and " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(expr.right->type) + ANSI_RESET,
                expr.span
            );
        }

        expr.type = parser_.getBaseTypes().boolean;
        if (expr.left->known && expr.right->known)
            expr.known = true;

        break;
    }
    }
}

void Analyzer::validate(Ast::UnaryExpr& node) {
    switch (node.code) {
    case Ast::ExprEnum::Negate: {
        if (node.val->type->code != Ast::TypeEnum::Int && node.val->type->code != Ast::TypeEnum::Real) {
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
    case Ast::ExprEnum::Not: {
        if (node.val->type->code != Ast::TypeEnum::Bool) {
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
        throw std::runtime_error("invalid Ast::UnaryExpr.code " + std::to_string(static_cast<int>(node.code)));
    }
}

void Analyzer::validate(Ast::PrintStmt& node) {
    for (auto& arg: node.args)
        arg->validate(*this);
}

void Analyzer::validate(Ast::IfStmt& node) {
    node.condition->validate(*this);
    node.body->validate(*this);
    if (node.elseBody)
        node.elseBody->validate(*this);
}

void Analyzer::validate(Ast::WhileStmt& node) {
    node.condition->validate(*this);
    node.body->validate(*this);
}

void Analyzer::validate(Ast::ForStmt& node) {
    node.body->declMap.emplace(node.counter->id, node.counter);
    node.counter->type = parser_.getBaseTypes().integer;
    node.range->validate(*this);
    node.body->validate(*this);
}

void Analyzer::validate(Ast::ReturnStmt& node) {
    if (!currRoutine_) {
        saveError("return statement outside of routine body", node.span);
        return;
    }

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

void Analyzer::validate(Ast::Assignment& node) {
    node.left->validate(*this);
    if (node.left->type->code == Ast::TypeEnum::Routine) {
        saveError("cannot assign to routine identifier", node.left->span);
        return;
    }

    node.val->validate(*this);
    if (node.val->type->code == Ast::TypeEnum::ERROR)
        return;

    if (areTypesEqual(node.val->type, node.left->type))
        return;

    if (node.left->type->code == Ast::TypeEnum::Array || node.left->type->code == Ast::TypeEnum::Record) {
        saveError("assignment type mismatch\n\texpected: "
            + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + stringifyType(node.left->type)
            + ANSI_RESET + "\n\tbut got: "
            + ANSI_START ANSI_BOLD ANSI_APPLY + stringifyType(node.val->type) + ANSI_RESET,
            node.val->span
        );
        return;
    }

    if (node.left->type->code == Ast::TypeEnum::Bool) {
        if (node.val->type->code == Ast::TypeEnum::Real) {
            saveError("illegal assignment of real to boolean", node.val->span);
            return;
        }
        if (node.val->type->code == Ast::TypeEnum::Int) {
            if (!node.val->known) {
                // TODO
                return;
            }
            // TODO
        }
    }
}

void Analyzer::validate(Ast::Var& node) {
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
        if (node.type->code == Ast::TypeEnum::REFERENCE) {
            auto lock = reinterpret_cast<Ast::TypeRef&>(*node.type).ref.lock();
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

        node.type = node.val->type;
    }
}

// Check that routine was declared in global scope
void Analyzer::validate(Ast::Routine& node) {
    if (currBlock_ != root_.get()) {
        saveError(
            "routine declarations are only allowed in global scope",
            node.span
        );
        return;
    }

    if (!node.body) {
        undefinedRoutines_.emplace(node.id, std::static_pointer_cast<Ast::Routine>(node.shared_from_this()));
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
    currBlock_ = node.body.get();
    for (auto& param: node.getType()->params)
        param->validate(*this);

    currRoutine_ = &node;
    node.body->validate(*this);
    currRoutine_ = nullptr;
}

void Analyzer::validate(Ast::RoutineCall& node) {
    auto& expr = static_cast<Ast::RoutineCall&>(node);
    auto decl = searchDeclaration(expr.routineId);
    if (!decl) {
        saveError("use of undeclared identifier: "
            + std::string(ANSI_START ANSI_BOLD ANSI_APPLY) + expr.routineId + ANSI_RESET,
            expr.span
        );
        return;
    }
    if (!decl->isRoutine) {
        saveError("attempt to call a variable that is not a routine " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
            + expr.routineId + ANSI_RESET,
            expr.span
        );
        return;
    }

    auto& routine = static_cast<Ast::Routine&>(*decl);
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
    }
    if (!routine.body) {
        saveError("use of routine with no body: " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
            + expr.routineId + ANSI_RESET,
            expr.span
        );
    }

    node.type = routine.getType()->retType;
    node.ref = std::static_pointer_cast<Ast::Routine>(decl);
}

void Analyzer::validate(Ast::ArrayType& node) {
    // TODO
    node.elemType->validate(*this);
}

void Analyzer::validate(Ast::ArrayAccess& node) {
    if ((*idRef_.currType)->code != Ast::TypeEnum::Array) {
        idRef_.currType = &parser_.getBaseTypes().error;
        saveError(
            "type " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
            + stringifyType(*idRef_.currType) + ANSI_RESET + "is not an array",
            node.span
        );
        return;
    }
    idRef_.currType = &std::static_pointer_cast<Ast::ArrayType>(*idRef_.currType)->elemType;

    {
        auto state = idRef_;
        idRef_.head = nullptr;
        node.val->validate(*this);
        idRef_ = state;
    }
    if (node.val->known) {
        // TODO
    }

    if (node.next) {
        idRef_.prev = &node;
        node.next->validate(*this);
    }
}

void Analyzer::validate(Ast::RecordType& node) {
    for (auto& member: node.members) {
        if (member->type)
            member->type->validate(*this);
        if (member->val) {
            saveError("record type members cannot have initializers", member->val->span);
            member->type = parser_.getBaseTypes().error;
        }
    }
}