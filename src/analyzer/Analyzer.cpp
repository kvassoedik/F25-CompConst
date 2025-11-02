#include "analyzer/Analyzer.h"
#include "parser/Parser.h"
#include "utils/PrintingUtils.h"

int Analyzer::configure(int* argc, char** argv) {
    return 0;
}

void Analyzer::run() {
    root_ = parser_.getRoot();
    currBlock_ = root_.get();
    validate(*currBlock_);
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

std::string Analyzer::stringifyType(const std::shared_ptr<Ast::Type>& t) {
    return "_TODO_";
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

}

void Analyzer::validate(Ast::ArrayIdRange& node) {
    

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
    } else {
        auto it = std::find_if(
            static_cast<Ast::RecordType&>(**idRef_.currType).members.begin(),
            static_cast<Ast::RecordType&>(**idRef_.currType).members.end(),
            [&node](std::shared_ptr<Ast::Var> var) { return var->id == node.id; }
        );
        if (it == static_cast<Ast::RecordType&>(**idRef_.currType).members.end()) {
            idRef_.head = nullptr;
            saveError("type " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
                + stringifyType(*idRef_.currType) + ANSI_RESET + " does not have a field '"
                + ANSI_START ANSI_BOLD ANSI_APPLY + node.id + ANSI_RESET + "'",
                node.span
            );
            return;
        }

        idRef_.currType = &(*it)->type;
    }

    if (node.next) {
        if ((*idRef_.currType)->code != Ast::TypeEnum::Record) {
            saveError("object " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
                + node.id + ANSI_RESET
                + " is not of record type; actual type: " + stringifyType(*idRef_.currType),
                node.span
            );
            return;
        }

        node.next->validate(*this);
    } else {
        try {
            idRef_.head->type = *idRef_.currType;
            idRef_.head = nullptr;
        } catch(...) {
            idRef_.head = nullptr;
            throw std::runtime_error("exception when validating IdRef");
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

}

void Analyzer::validate(Ast::PrintStmt& node) {
    for (auto& arg: node.args)
        arg->validate(*this);
}

void Analyzer::validate(Ast::IfStmt& node) {

}

void Analyzer::validate(Ast::WhileStmt& node) {

}

void Analyzer::validate(Ast::ForStmt& node) {

}

void Analyzer::validate(Ast::ReturnStmt& node) {

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
            + stringifyType(node.left->type)
            + "\n\tbut got: " + stringifyType(node.val->type),
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
                + "\n\texpected: " + stringifyType(node.type)
                + "\n\tbut got: " + stringifyType(node.val->type),
                node.span
            );
            return;
        }

        node.type = node.val->type;
    }

    currBlock_->declMap.emplace(node.id, node.shared_from_this());
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

    // TODO no body
    currBlock_->declMap.emplace(node.id, node.shared_from_this());
    node.body->validate(*this);
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
        if (!areTypesEqual(routine.getType()->params[i]->type, expr.args[i]->type)) {
            saveError(std::string("in routine call to ") + ANSI_START ANSI_BOLD ANSI_APPLY + routine.id
                + ANSI_RESET + ": argument #" + std::to_string(i) + " type mismatch\n\t"
                + "expected: " + stringifyType(routine.getType()->params[i]->type)
                + "\n\tbut got: " + stringifyType(expr.args[i]->type),
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
}

void Analyzer::validate(Ast::ArrayType& node) {
    node.elemType->validate(*this);
}

void Analyzer::validate(Ast::ArrayAccess& node) {
    if ((*idRef_.currType)->code != Ast::TypeEnum::Array) {
        saveError("type " + std::string(ANSI_START ANSI_BOLD ANSI_APPLY)
            + stringifyType(*idRef_.currType) + ANSI_RESET + "is not an array",
            node.span
        );
        return;
    }

    node.val->validate(*this);
    if (node.val->known) {
        // TODO
        return;
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