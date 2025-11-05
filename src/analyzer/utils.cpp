#include "analyzer/utils.h"
#include "parser/Ast.h"
#include <sstream>

bool analyzer::isPrimitiveType(const std::shared_ptr<Ast::Type>& t) {
    return t->code == Ast::TypeEnum::Bool || t->code == Ast::TypeEnum::Int || t->code == Ast::TypeEnum::Real;
}

bool analyzer::areTypesEqual(const std::shared_ptr<Ast::Type>& t1, const std::shared_ptr<Ast::Type>& t2) {
    if (t1->code != t2->code)
        return false;

    switch (t1->code) {
    case Ast::TypeEnum::Array: {
        const auto& at1 = static_cast<Ast::ArrayType&>(*t1), at2 = static_cast<Ast::ArrayType&>(*t2);
        if (at1.size && at2.size) {
            if (!at1.size->knownPrimitive || !at2.size->knownPrimitive
                || at1.size->code != Ast::ExprEnum::IntLiteral
                || at2.size->code != Ast::ExprEnum::IntLiteral
                || static_cast<Ast::IntLiteral&>(*at1.size).val != static_cast<Ast::IntLiteral&>(*at2.size).val
            )
                return false;
        }

        if (!at1.elemType || !at2.elemType || !areTypesEqual(at1.elemType, at2.elemType))
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

bool analyzer::isErrorType(const std::shared_ptr<Ast::Type> type) { return type->code == Ast::TypeEnum::ERROR; }

std::string analyzer::stringifyType(const std::shared_ptr<Ast::Type>& t) {
    std::stringstream out;
    t->printType({.os = out});
    return out.str();
}