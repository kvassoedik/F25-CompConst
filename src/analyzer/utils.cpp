#include "analyzer/utils.h"
#include "parser/Ast.h"
#include "parser/structs.h"
#include <sstream>

using namespace ast;

bool analyzer::isPrimitiveType(const ast::Type& t) {
    return t.code == TypeEnum::Bool || t.code == TypeEnum::Int || t.code == TypeEnum::Real;
}

ast::Type& analyzer::getPureType(ast::Type& t) {
    if (t.code == TypeEnum::REFERENCE) {
        auto lock = static_cast<const TypeRef&>(t).ref.lock();
        if (!lock)
            throw std::runtime_error("getPureType: ref == nullptr on " + static_cast<const TypeRef&>(t).id + " TypeRef\n");
        return *lock->type;
    }
    return t;
}
const ast::Type& analyzer::getPureType(const ast::Type& t) {
    if (t.code == TypeEnum::REFERENCE) {
        auto lock = static_cast<const TypeRef&>(t).ref.lock();
        if (!lock)
            throw std::runtime_error("getPureType: ref == nullptr on " + static_cast<const TypeRef&>(t).id + " TypeRef\n");
        return *lock->type;
    }
    return t;
}

bool analyzer::areTypesEqual(Type& ty1, Type& ty2) {
    auto &t1 = getPureType(ty1), &t2 = getPureType(ty2);
    if (t1.code != t2.code)
        return false;

    switch (t1.code) {
    case TypeEnum::Bool:
    case TypeEnum::Int:
    case TypeEnum::Real: return true;
    case TypeEnum::Array: {
        const auto &at1 = static_cast<ArrayType&>(t1), &at2 = static_cast<ArrayType&>(t2);
        if (at1.size && at2.size) {
            if (!at1.size->knownPrimitive || !at2.size->knownPrimitive
                || at1.size->code != ExprEnum::IntLiteral
                || at2.size->code != ExprEnum::IntLiteral
                || static_cast<IntLiteral&>(*at1.size).val != static_cast<IntLiteral&>(*at2.size).val
            )
                return false;
        }

        if (!at1.elemType || !at2.elemType || !areTypesEqual(*at1.elemType, *at2.elemType))
            return false;
        return true;
    }
    case TypeEnum::Record: {
        const auto &at1 = static_cast<RecordType&>(t1), &at2 = static_cast<RecordType&>(t2);
        if (at1.members.size() != at2.members.size()) {
            std::cerr << at1.members.size() << " " << at2.members.size() << "\n";
            return false;
        }

        for (size_t i = 0; i < at1.members.size(); ++i) {
            if (!areTypesEqual(*at1.members[i]->type, *at2.members[i]->type))
                return false;
        }
        return true;
    }
    case TypeEnum::Routine: {
        const auto &at1 = static_cast<RoutineType&>(t1), &at2 = static_cast<RoutineType&>(t2);
        if (at1.params.size() != at2.params.size())
            return false;

        for (size_t i = 0; i < at1.params.size(); ++i) {
            if (!areTypesEqual(*at1.params[i]->type, *at2.params[i]->type))
                return false;
        }

        if (!at1.retType)
            if (!at2.retType) return true;
            else return false;
        if (!at2.retType)
            if (!at1.retType) return true;
            else return false;
        if (!areTypesEqual(*at1.retType, *at2.retType))
            return false;

        return true;
    }
    }
    
    return false;
}

bool analyzer::isErrorType(const Type& type) { return type.code == TypeEnum::ERROR; }

std::string analyzer::stringifyType(const Type& t) {
    std::stringstream out;
    t.serializeType({.os = out});
    return out.str();
}