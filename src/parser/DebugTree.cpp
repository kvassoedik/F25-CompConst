#include "parser/DebugTree.h"
#include "parser/Ast.h"

#define AST_DEBUG_PRINT_METHOD_IMPL_TAIL(span) \
"   " ANSI_START ANSI_BLUE ANSI_APPLY << span << ANSI_RESET "\n"

#define AST_DEBUG_PRINT_METHOD(_stmt_) \
AST_DEBUG_PRINT_METHOD_SIGNATURE override {\
    os << _stmt_ << AST_DEBUG_PRINT_METHOD_IMPL_TAIL;\
}

#define AST_DEBUG_PTR_TO_STR(_ptr_) \
(_ptr_ \
? (debugInfo.pushPrint(_ptr_->debugId),\
    std::string("^").append(ANSI_START ANSI_YELLOW ANSI_APPLY).append(std::to_string(_ptr_->debugId)).append(ANSI_RESET)) \
: ("^" ANSI_START ANSI_YELLOW ANSI_APPLY "N" ANSI_RESET))


using namespace Ast;

DebugTree::DebugTree()
    : os_(std::cout)
{
    newline_.reserve(32);
    nodes_.reserve(1024);
    nodes_.emplace_back(nullptr);
}

void DebugTree::newNode(std::shared_ptr<Ast::Entity> node) {
    node->debugId = globalDebugId_++;
    nodes_.push_back(std::move(node));
}

void DebugTree::printAll() {
    std::cout << ANSI_START ANSI_GREEN ANSI_APPLY << std::string(28, '-') << " AST_DEBUG " << std::string(28, '-') << ANSI_RESET "\n";
    for (size_t i = 1; i < nodes_.size(); i++) {
        while (!depthStack_.empty()) {
            auto [debugId, depth] = depthStack_.top();
            depthStack_.pop();

            if (depth < depth_) {
                std::cout << ANSI_START ANSI_RED ANSI_APPLY "|\n" ANSI_RESET;
            }
            depth_ = depth;

            printImpl(
                nodes_[debugId],
                std::string(ANSI_START ANSI_GREEN ANSI_APPLY "[").append(std::to_string(debugId)).append("]  " ANSI_RESET)
            );
            if (depthIncrement_) {
                depth_++;
                depthIncrement_ = false;
            }
        }
        depth_ = 0;

        if (alreadyDisplayed_.find(i) != alreadyDisplayed_.end())
            continue;
        alreadyDisplayed_.emplace(i);
        printImpl(
            nodes_[i],
            std::string(ANSI_START ANSI_RED ANSI_APPLY "[").append(std::to_string(i)).append("]  " ANSI_RESET)
        );
    }
}

void DebugTree::pushPrint(unsigned long debugId) {
    if (debugId == 0 || alreadyDisplayed_.find(debugId) != alreadyDisplayed_.end())
        return;
    alreadyDisplayed_.emplace(debugId);
    depthStack_.emplace(debugId, depth_ + 1);
    depthIncrement_ = true;
}

void DebugTree::printImpl(const std::shared_ptr<Ast::Entity>& node, const std::string& prefix) {
    size_t size = depth_+1 + prefix.size();
    newline_.reserve(size);
    newline_.assign(depth_, '-');
    if (depth_ > 0)
        newline_[0] = '*';

    std::cout << ANSI_START ANSI_RED ANSI_APPLY << newline_ << ANSI_RESET << prefix;
    newline_.assign(size, ' ');
    newline_[0] = '\n';
    node->print(*this);
}

void DebugTree::print(Ast::Entity& node) {
    os_ << "<entity>"
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// ---------- Node-specific printing methods ----------

// void print(Ast::Type& node) {
//     std::string output;
//     switch(node.code) {
//         case TypeEnum::ERROR: {output = "<error>"; break;}
//         case TypeEnum::RESOLVABLE: {output = "<processed>"; break;}
//         case TypeEnum::Int: {output = "integer"; break;}
//         case TypeEnum::Real: {output = "real"; break;}
//         case TypeEnum::Bool: {output = "boolean"; break;}
//         case TypeEnum::Array: {output = "array"; break;}
//         case TypeEnum::Record: {output = "record"; break;}
//         default: {output = "INVALID_" + std::to_string(static_cast<int>(node.code)); break;}
//     }
//     os_ << "Type:" << output << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
// }

void DebugTree::print(Ast::Block& node) {
    std::string sUnits, sDecls, sTypes;

    for (auto& unit: node.units) {
        sUnits += AST_DEBUG_PTR_TO_STR(unit);
        sUnits += ",";
    }
    if (!sUnits.empty())
        sUnits.pop_back();

    for (auto&& it = node.declMap.begin(); it != node.declMap.end(); ++it) {
        sDecls += AST_DEBUG_PTR_TO_STR((&it->second));
        sDecls += ",";
    }
    if (!sDecls.empty())
        sDecls.pop_back();

    for (auto&& it = node.typeMap.begin(); it != node.typeMap.end(); ++it) {
        sTypes += AST_DEBUG_PTR_TO_STR((&it->second));
        sTypes += ",";
    }
    if (!sTypes.empty())
        sTypes.pop_back();
    
    std::string pd(2, ' ');
    os_ << "Block {"
        << newline_ << pd << "parent " << AST_DEBUG_PTR_TO_STR(node.parent)
        << newline_ << pd << "units: " << sUnits
        << newline_ << pd << "decls: " << sDecls
        << newline_ << pd << "types: " << sTypes
        << newline_ << "}" << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// === Base / common ===
void DebugTree::print(Ast::Type& node) {
    os_ << "Type_base " << static_cast<int>(node.code)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

void DebugTree::print(Ast::TypeRef& node) {
    os_ << "TypeRef " << node.id
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

void DebugTree::print(Ast::TypeDecl& node) {
    os_ << "type " << node.id << " is " << AST_DEBUG_PTR_TO_STR(node.type)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// === Fallbacks ===
void DebugTree::print(Ast::Expr& node) {
    os_ << "Expr_base " << static_cast<int>(node.code)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Ast::RangeSpecifier& node) {
    os_ << "RangeSpecifier_base"
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// === Ranges / ArrayId ===
void DebugTree::print(Ast::IntRange& node) {
    os_ << AST_DEBUG_PTR_TO_STR(node.start) << " .. "
        << AST_DEBUG_PTR_TO_STR(node.end)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Ast::ArrayId& node) {
    os_ << "ArrayId " << node.id
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// === Primaries ===
void DebugTree::print(Ast::ModifiablePrimary& node) {
    os_ << "ModifiablePrimary_base -> " << AST_DEBUG_PTR_TO_STR(node.next)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Ast::IdRef& node) {
    os_ << "IdRef " << node.id
        << (node.next ? " -> " + AST_DEBUG_PTR_TO_STR(node.next) : "")
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// === Literals / Expressions ===
void DebugTree::print(Ast::BoolLiteral& node) {
    os_ << (node.val ? "true" : "false")
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Ast::IntLiteral& node) {
    os_ << "int " << node.val
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Ast::RealLiteral& node) {
    os_ << "real " << node.val
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

void DebugTree::print(Ast::BinaryExpr& node) {
    using E = Ast::ExprEnum;
    std::string op;
    switch (node.code) {
        case E::Add: op = "+"; break;
        case E::Subtract: op = "-"; break;
        case E::Multiply: op = "*"; break;
        case E::Divide: op = "/"; break;
        case E::Modulo: op = "%"; break;
        case E::And: op = "and"; break;
        case E::Or: op = "or"; break;
        case E::Xor: op = "xor"; break;
        case E::LESS_THAN: op = "<"; break;
        case E::LESS_OR_EQUAL: op = "<="; break;
        case E::MORE_THAN: op = ">"; break;
        case E::MORE_OR_EQUAL: op = ">="; break;
        case E::EQUAL: op = "="; break;
        case E::UNEQUAL: op = "/="; break;
        default: op = "INVALID_"; break;
    }
    os_ << AST_DEBUG_PTR_TO_STR(node.left) << " "
        << op << " "
        << AST_DEBUG_PTR_TO_STR(node.right)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

void DebugTree::print(Ast::UnaryExpr& node) {
    using E = Ast::ExprEnum;
    std::string op;
    switch (node.code) {
        case E::Negate: op = "-"; break;
        case E::Not:    op = "not "; break;
        default:        op = "INVALID_ "; break;
    }
    os_ << op << AST_DEBUG_PTR_TO_STR(node.val)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// === Statements ===
void DebugTree::print(Ast::Decl& node) {
    os_ << "decl " << node.id
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

void DebugTree::print(Ast::PrintStmt& node) {
    std::string sArgs;
    for (size_t i = 0; i < node.args.size(); ++i) {
        sArgs += AST_DEBUG_PTR_TO_STR(node.args[i]);
        if (i + 1 < node.args.size()) sArgs += ", ";
    }
    os_ << "print(" << sArgs << ")"
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

void DebugTree::print(Ast::IfStmt& node) {
    os_ << "if " << AST_DEBUG_PTR_TO_STR(node.condition)
        << " then " << AST_DEBUG_PTR_TO_STR(node.body);
    if (node.elseBody) os_ << " else " << AST_DEBUG_PTR_TO_STR(node.elseBody);
    os_ << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Ast::WhileStmt& node) {
    os_ << "while " << AST_DEBUG_PTR_TO_STR(node.condition)
        << " loop " << AST_DEBUG_PTR_TO_STR(node.body)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Ast::ForStmt& node) {
    os_ << "for " << node.counterId
        << " in " << AST_DEBUG_PTR_TO_STR(node.range)
        << (node.reverse ? " reverse " : " ")
        << "loop " << AST_DEBUG_PTR_TO_STR(node.body)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Ast::ReturnStmt& node) {
    os_ << "return " << AST_DEBUG_PTR_TO_STR(node.val)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Ast::Assignment& node) {
    os_ << AST_DEBUG_PTR_TO_STR(node.left)
        << " := " << AST_DEBUG_PTR_TO_STR(node.val)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// === Decls / Routine / Calls ===
void DebugTree::print(Ast::Var& node) {
    os_ << "var " << node.id << ": "
        << AST_DEBUG_PTR_TO_STR(node.type)
        << " is " << AST_DEBUG_PTR_TO_STR(node.val)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Ast::Routine& node) {
    std::string sParams;
    for (size_t i = 0; i < node.params.size(); ++i) {
        sParams += AST_DEBUG_PTR_TO_STR(node.params[i]);
        if (i + 1 < node.params.size()) sParams += ",";
    }
    os_ << "routine " << node.id << "(" << sParams << "): "
        << AST_DEBUG_PTR_TO_STR(node.retType)
        << " is " << AST_DEBUG_PTR_TO_STR(node.body)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Ast::RoutineCall& node) {
    std::string sArgs;
    for (size_t i = 0; i < node.args.size(); ++i) {
        sArgs += AST_DEBUG_PTR_TO_STR(node.args[i]);
        if (i + 1 < node.args.size()) sArgs += ",";
    }
    os_ << "RoutineCall " << node.routineId
        << " (" << sArgs << ")"
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// === Array / Record ===
void DebugTree::print(Ast::ArrayType& node) {
    os_ << "array[" << AST_DEBUG_PTR_TO_STR(node.size) << "]: "
        << AST_DEBUG_PTR_TO_STR(node.elemType)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Ast::ArrayAccess& node) {
    os_ << "ArrayAccess [" << AST_DEBUG_PTR_TO_STR(node.val) << "]"
        << (node.next ? " ->" + AST_DEBUG_PTR_TO_STR(node.next) : "")
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Ast::RecordType& node) {
    std::string sMembers;
    for (size_t i = 0; i < node.members.size(); ++i) {
        sMembers += AST_DEBUG_PTR_TO_STR(node.members[i]);
        if (i + 1 < node.members.size()) sMembers += ",";
    }
    os_ << "record {" << sMembers << "}"
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}