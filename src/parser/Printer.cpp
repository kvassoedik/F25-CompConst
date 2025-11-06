#include "parser/Printer.h"
#include "parser/Ast.h"
#include "parser/structs.h"
#include <sstream>

using namespace ast;
using ast::Printer::options;

void Printer::printType(Type& node, options o) {
    std::string output;
    switch(node.code) {
        case TypeEnum::ERROR: {output = "<error>"; break;}
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

void Printer::printType(TypeRef& node, options o) {
    std::stringstream output;
    output << node.id << " (aka ";
    auto lock = node.ref.lock();
    if (lock)
        lock->type->printType({.os = output});
    else
        output << "??";
    output << ')';
    o.os << output.str();
}

void Printer::printType(RoutineType& node, options o) {
    std::stringstream output;
    output << "routine(";
    for (size_t i = 0; i < node.params.size(); ++i) {
        node.params[i]->type->printType({.os = output});
        if (i + 1 < node.params.size()) output << ", ";
    }

    output << "): ";
    if (node.retType)
        node.retType->printType({.os = output});
    else
        output << "null";

    o.os << output.str();
}

void Printer::printType(ArrayType& node, options o) {
    std::stringstream output;
    output << "array[";
    if (node.size) {
        output << (node.size->knownPrimitive ? std::to_string(static_cast<IntLiteral&>(*node.size).val) : "??");
    }
    output << "]: ";
    node.elemType->printType({.os = output});
    o.os << output.str();
}

void Printer::printType(RecordType& node, options o) {
    std::stringstream output;
    output << "record{";
    for (size_t i = 0; i < node.members.size(); ++i) {
        node.members[i]->type->printType({.os = output});
        if (i + 1 < node.members.size()) output << ", ";
    }
    output << "}";
    o.os << output.str();
}