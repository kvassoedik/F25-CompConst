#include "parser/Printer.h"
#include "parser/Ast.h"
#include <sstream>

using namespace Ast;

void Printer::printType(Ast::Type& node, Printer::options o) {
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

void Printer::printType(Ast::TypeRef& node, Printer::options o) {
    std::stringstream output;
    output << node.id << " (aka ";
    auto lock = node.ref.lock();
    if (lock)
        lock->type->printType(*this, {.os = output});
    else
        output << "??";
    output << ')';
    o.os << output.str();
}

void Printer::printType(Ast::RoutineType& node, Printer::options o) {
    std::stringstream output;
    output << "routine(";
    for (size_t i = 0; i < node.params.size(); ++i) {
        node.params[i]->type->printType(*this, {.os = output});
        if (i + 1 < node.params.size()) output << ", ";
    }

    output << "): ";
    if (node.retType)
        node.retType->printType(*this, {.os = output});
    else
        output << "null";

    o.os << output.str();
}

void Printer::printType(Ast::ArrayType& node, Printer::options o) {
    std::stringstream output;
    output << "array[";
    if (node.size) {
        output << (node.size->knownPrimitive ? "_" : "??");
    }
    output << "]: ";
    node.elemType->printType(*this, {.os = output});
    o.os << output.str();
}

void Printer::printType(Ast::RecordType& node, Printer::options o) {
    std::stringstream output;
    output << "record{";
    for (size_t i = 0; i < node.members.size(); ++i) {
        node.members[i]->type->printType(*this, {.os = output});
        if (i + 1 < node.members.size()) output << ", ";
    }
    output << "}";
    o.os << output.str();
}