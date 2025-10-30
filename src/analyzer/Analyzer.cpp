#include "analyzer/Analyzer.h"
#include "parser/Ast.h"
#include "utils/PrintingUtils.h"

int Analyzer::configure(int* argc, char** argv) {
    return 0;
}

void Analyzer::run() {
    currBlock_ = root_;
    currBlock_->validate(*this);
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
        const auto& at1 = static_cast<Ast::ArrayType>(&*t1), at2 = static_cast<Ast::ArrayType>(&*t2);
        if (at1->size && at2->size) {
            if (!at1->size.known || !at2->size.known
                || !at1->size.code != ExprEnum::IntLiteral
                || !at2->size.code != ExprEnum::IntLiteral
                || static_cast<Ast::IntLiteral>(&*t1->size) != static_cast<Ast::IntLiteral>(&*t2->size)
            )
                return false;
        }

        if (!at1->elemType
            || !at2->elemType
            || !areTypesEqual(at1->elemType, at2->elemType)
        )
            return false;
        break;
    }
    case Ast::TypeEnum::Record: {
        const auto& at1 = static_cast<Ast::RecordType>(&*t1), at2 = static_cast<Ast::RecordType>(&*t2);
        if (at1->members.size() != at2->members.size())
            return false;

        for (size_t i = 0; i < at1->members.size(); ++i) {
            if (!areTypesEqual(at1[i], at2[i]))
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
    std::shared_ptr<Ast::Decl> decl = nullptr;
    auto* block = &currBlock_;
    while (block) {
        auto it = (*block)->declMap.find(node.id);
        if (it != (*block)->declMap.end()) {
            decl = std::move(it->second);
            break;
        }
        block = &currBlock_->parent;
    }

    return decl;
}

// Check that identifier was declared
void Analyzer::validate(Ast::IdRef& node) {
    auto decl = searchDeclaration(node.id);
    if (!decl) {
        saveError(
            std::string("use of undeclared identifier: " ANSI_START ANSI_BOLD ANSI_APPLY) + node.id + ANSI_RESET,
            node.span
        );
        return;
    }
    
    decl->validate(*this);
    if (node.next)
        node.next->validate(*this);
}

void Analyzer::validate(Ast::Var& node) {
    if (node.val) {
        node.val->validate(*this);
        if (node.type && node.val->type && !areTypesEqual(node.type, node.val->type)) {
            saveError(
                "type mismatch\n\texpected: "
                + stringifyType(node.type)
                + "\n\tbut got: " + stringifyType(node.val->type),
                node.span
            );
            return;
        }
    }

    currBlock_->declMap.emplace(node->id, std::move(node));
}

// Check that routine was declared in global scope
void Analyzer::validate(Ast::Routine& node) {
    if (currBlock_ != root_) {
        saveError(
            "routine declarations are only allowed in global scope",
            node.span
        );
        return;
    }

    // TODO no body
    currBlock_ = node.body;
    currBlock_->declMap.emplace(node->id, node);
    node.body->validate(*this);
    currBlock_ = currBlock_->parent;
}

void Analyzer::validate(Ast::Block& node) {
    for (auto& unit: node.units) {
        unit->validate(*this);
    }
}

void Analyzer::validate(Ast::Expr& node) {
    switch (node.code) {
    case Ast::ExprEnum::BoolLiteral: { node.type = Ast::TypeEnum::Bool; node.known = true; break; }
    case Ast::ExprEnum::IntLiteral: { node.type = Ast::TypeEnum::Int; node.known = true; break; }
    case Ast::ExprEnum::RealLiteral: { node.type = Ast::TypeEnum::Real; node.known = true; break; }
    case Ast::ExprEnum::IdRef: { validate(static_cast<Ast::IdRef>(&**node)); break; }
    case Ast::ExprEnum::ArrayAccess: { validate(static_cast<Ast::ArrayAccess>(&**node)); break; }
    case Ast::ExprEnum::Add:
    case Ast::ExprEnum::Subtract:
    case Ast::ExprEnum::Multiply:
    case Ast::ExprEnum::Divide:
    case Ast::ExprEnum::Modulo:
    case Ast::ExprEnum::LESS_THAN:
    case Ast::ExprEnum::LESS_OR_EQUAL:
    case Ast::ExprEnum::MORE_THAN:
    case Ast::ExprEnum::MORE_OR_EQUAL: {
        auto& expr = static_cast<Ast::BinaryExpr>(&*node);

        validate(*expr.left);
        if (expr.left->type->code == Ast::TypeEnum::ERROR)
            return;
        
        validate(*expr.right);
        if (expr.right->type->code == Ast::TypeEnum::ERROR)
            return;

        if ((expr.left->type->code != Ast::TypeEnum::Int && expr.left->type->code != Ast::TypeEnum::Real)
            || (expr.right->type->code != Ast::TypeEnum::Int && expr.right->type->code != Ast::TypeEnum::Real)
        ) {
            saveError("binary operator used with incompatible types: " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(expr.left->type)
                + ANSI_RESET + "and " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(expr.right->type) + ANSI_RESET,
                expr.>span
            );
        }

        // compile-time computation
        if (expr.left->known && expr.right->known) {
            switch (node.code) {
            case Ast::ExprEnum::Add: {
                
                break;
            }
            default:
                break;
            }
        }

        break;
    }
    case Ast::ExprEnum::EQUAL:
    case Ast::ExprEnum::UNEQUAL: {
        auto& expr = static_cast<Ast::BinaryExpr>(&*node);

        validate(*expr.left);
        if (expr.left->type->code == Ast::TypeEnum::ERROR)
            return;
        
        validate(*expr.right);
        if (expr.right->type->code == Ast::TypeEnum::ERROR)
            return;

        if (!areTypesEqual(expr.left->type, expr.right->type)) {
            saveError("equality used with incompatible types: " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(expr.left->type)
                + ANSI_RESET + "and " + ANSI_START ANSI_BOLD ANSI_APPLY
                + stringifyType(expr.right->type) + ANSI_RESET,
                expr.span
            );
        }
        break;
    }
    case Ast::ExprEnum::RoutineCall: {
        auto& expr = static_cast<Ast::RoutineCall>(&*node);
        auto& decl = searchDeclaration(expr.routineId);
        if (!decl) {
            saveError(
                std::string("use of undeclared identifier: ") + ANSI_START ANSI_BOLD ANSI_APPLY + expr.routineId + ANSI_RESET,
                expr.span
            );
            return;
        }
        if (!decl->isRoutine) {
            saveError("attempt to call a non-routine identifier " + ANSI_START ANSI_BOLD ANSI_APPLY
                + expr.routineId + ANSI_RESET
                expr.span
            );
            return;
        }

        auto& routine = static_cast<Ast::RoutineCall>(&**decl);
        if (routine.params.size() != expr.args.size()) {
            saveError("routine call argument count mismatch", expr.span);
        }
        for (size_t i = 0; i < routine.params.size(); ++i) {
            if (!areTypesEqual(routine.params[i]->type, expr.args[i]->type)) {
                saveError(std::string("in routine call to ") + ANSI_START ANSI_BOLD ANSI_APPLY + routine.id
                + ANSI_RESET + ": argument #" + std::to_string(i) + " type mismatch\n\t"
                + "expected: " + stringifyType(routine.params[i]->type)
                + "\n\tbut got: " + stringifyType(expr.args[i]->type),
                expr.args[i]->span
            );
            }
        }
        if (!routine->body) {
            saveError("use of routine with no body: " + ANSI_START ANSI_BOLD ANSI_APPLY
                + expr.routineId + ANSI_RESET
                expr.span
            );
        }

        node.type = routine.retType;

        break;
    }
    }
}

void Analyzer::validate(Ast::IfStmt& node) {

}