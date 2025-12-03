#include "parser/Ast.h"
#include "parser/structs.h"
#include <sstream>

using namespace ast;
using ast::srlz::options;

void srlz::type(const Type& node, options o) {
    std::string output;
    switch(node.code) {
        case TypeEnum::ERROR: {output = "<error>"; break;}
        case TypeEnum::NONE: {output = "<none>"; break;}
        case TypeEnum::Int: {output = "integer"; break;}
        case TypeEnum::Real: {output = "real"; break;}
        case TypeEnum::Bool: {output = "boolean"; break;}
        case TypeEnum::Array: {output = "array??"; break;}
        case TypeEnum::Record: {output = "record??"; break;}
        case TypeEnum::Routine: {output = "routine??"; break;}
        default: {output = "INVALID_" + std::to_string(static_cast<int>(node.code)); break;}
    }
    o.os << output;
}

void srlz::type(const TypeRef& node, options o) {
    std::stringstream output;
    output << node.id << (o.ir ? "__" : " (aka ");
    auto lock = node.ref.lock();
    if (lock)
        lock->type->serializeType({.os = output});
    else
        output << "??";
    if (!o.ir)
        output << ')';
    o.os << output.str();
}

void srlz::type(const RoutineType& node, options o) {
    std::stringstream output;
    output << (o.ir ? "routine_" : "routine(");
    for (size_t i = 0; i < node.params.size(); ++i) {
        node.params[i]->type->serializeType({.os = output});
        if (i + 1 < node.params.size()) output << (o.ir ? "_" : ", ");
    }

    output << (o.ir ? "__" : "): ");
    if (node.retType)
        node.retType->serializeType({.os = output});
    else
        output << "null";

    o.os << output.str();
}

void srlz::type(const ArrayType& node, options o) {
    std::stringstream output;
    output << (o.ir ? "array_" : "array[");
    if (node.size) {
        output << (node.size->knownPrimitive ? std::to_string(static_cast<IntLiteral&>(*node.size).val) : "??");
    }
    output << (o.ir ? "_" : "]: ");
    node.elemType->serializeType({.os = output});
    o.os << output.str();
}

void srlz::type(const RecordType& node, options o) {
    std::stringstream output;
    output << (o.ir ? "record_" : "record{");
    for (size_t i = 0; i < node.members.size(); ++i) {
        node.members[i]->type->serializeType({.os = output});
        if (i + 1 < node.members.size()) output << ", ";
    }
    if (!o.ir)
        output << "}";
    o.os << output.str();
}