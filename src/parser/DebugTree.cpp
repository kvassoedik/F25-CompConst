#include "parser/DebugTree.h"
#include "parser/Ast.h"
#include "parser/structs.h"

#if AST_DEBUG_ON

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


using namespace ast;

DebugTree::DebugTree()
    : os_(std::cerr)
{
    newline_.reserve(32);
    nodes_.reserve(1024);
    nodes_.emplace_back(nullptr);
}

void DebugTree::newNode(std::shared_ptr<Entity> node) {
    node->debugId = globalDebugId_++;
    nodes_.push_back(std::move(node));
}

void DebugTree::printAll() {
    std::cerr << ANSI_START ANSI_GREEN ANSI_APPLY << std::string(28, '-') << " AST_DEBUG " << std::string(28, '-') << ANSI_RESET "\n";
    for (size_t i = 1; i < nodes_.size(); ++i) {
        while (!depthStack_.empty()) {
            auto top = depthStack_.top();
            depthStack_.pop();

            if (top.depth < depth_) {
                std::cerr << ANSI_START ANSI_RED ANSI_APPLY "|\n" ANSI_RESET;
            }
            depth_ = top.depth;

            printImpl(
                nodes_[top.debugId],
                std::string(ANSI_START) + (isCurrOrphan_ ? ANSI_RED : ANSI_GREEN) + ANSI_APPLY "[" + std::to_string(top.debugId) + "]  " + ANSI_RESET
            );
            if (depthIncrement_) {
                depth_++;
                depthIncrement_ = false;
            }
        }
        depth_ = 0;

        if (alreadyDisplayed_.find(i) != alreadyDisplayed_.end())
            continue;

        if (!isCurrOrphan_ && i > 8) {
            isCurrOrphan_ = true;
            std::cerr << "\n" << ANSI_START ANSI_GREEN ANSI_APPLY << std::string(14, '-') << " ORPHAN NODES " << std::string(14, '-') << ANSI_RESET "\n";
        }

        alreadyDisplayed_.emplace(i);
        printImpl(
            nodes_[i],
            std::string(ANSI_START ANSI_RED ANSI_APPLY "[").append(std::to_string(i)).append("]  " ANSI_RESET)
        );
    }
}

void DebugTree::pushPrint(unsigned long debugId) {
    if (alreadyDisplayed_.find(debugId) != alreadyDisplayed_.end())
        return;
    alreadyDisplayed_.emplace(debugId);
    depthStack_.emplace(debugId, depth_ + 1);
    depthIncrement_ = true;
}

void DebugTree::printImpl(const std::shared_ptr<Entity>& node, const std::string& prefix) {
    size_t size = depth_+1 + prefix.size();
    newline_.reserve(size);
    newline_.assign(depth_, '-');
    if (depth_ > 0)
        newline_[0] = '*';

    std::cerr << ANSI_START ANSI_RED ANSI_APPLY << newline_ << ANSI_RESET << prefix;
    newline_.assign(size, ' ');
    newline_[0] = '\n';
    node->print(*this);
}

void DebugTree::print(Entity& node) {
    os_ << "<entity>"
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// ---------- Node-specific printing methods ----------

void DebugTree::print(Block& node) {
    std::string sUnits, sDecls, sTypes;

    for (auto& unit: node.units) {
        sUnits += AST_DEBUG_PTR_TO_STR(unit);
        sUnits += ",";
    }
    if (!sUnits.empty())
        sUnits.pop_back();

    for (auto&& it = node.declMap.begin(); it != node.declMap.end(); ++it) {
        sDecls += AST_DEBUG_PTR_TO_STR(it->second);
        sDecls += ",";
    }
    if (!sDecls.empty())
        sDecls.pop_back();

    for (auto&& it = node.typeMap.begin(); it != node.typeMap.end(); ++it) {
        sTypes += AST_DEBUG_PTR_TO_STR(it->second);
        sTypes += ",";
    }
    if (!sTypes.empty())
        sTypes.pop_back();
    
    std::string pd(2, ' ');
    auto lock = node.parent.lock();

    os_ << "Block {"
        << newline_ << pd << "parent " << AST_DEBUG_PTR_TO_STR(lock)
        << newline_ << pd << "units: " << sUnits
        << newline_ << pd << "decls: " << sDecls
        << newline_ << pd << "types: " << sTypes
        << newline_ << "}" << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// === Base / common ===

void DebugTree::print(Type& node) {
    std::string output;
    switch(node.code) {
        case TypeEnum::ERROR: {output = "<error>"; break;}
        case TypeEnum::REFERENCE: {output = "<processed>"; break;}
        case TypeEnum::Int: {output = "integer"; break;}
        case TypeEnum::Real: {output = "real"; break;}
        case TypeEnum::Bool: {output = "boolean"; break;}
        case TypeEnum::Array: {output = "array"; break;}
        case TypeEnum::Record: {output = "record"; break;}
        default: {output = "INVALID_" + std::to_string(static_cast<int>(node.code)); break;}
    }
    os_ << "Type:" << output << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

void DebugTree::print(TypeRef& node) {
    auto lock = node.ref.lock();
    os_ << "TypeRef "
        << AST_DEBUG_PTR_TO_STR(lock) << " " << node.id
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

void DebugTree::print(TypeDecl& node) {
    os_ << "type " << node.id << " is " << AST_DEBUG_PTR_TO_STR(node.type)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// === Fallbacks ===
void DebugTree::print(Expr& node) {
    os_ << "Expr_base " << static_cast<int>(node.code)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(RangeSpecifier& node) {
    os_ << "RangeSpecifier_base"
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// === Ranges / ArrayIdRange ===
void DebugTree::print(IntRange& node) {
    os_ << AST_DEBUG_PTR_TO_STR(node.start) << " .. "
        << AST_DEBUG_PTR_TO_STR(node.end)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(ArrayIdRange& node) {
    auto lock = node.ref.lock();
    os_ << (!lock ? ANSI_START ANSI_RED ANSI_AND ANSI_BOLD ANSI_APPLY : "")
        << "ArrayIdRange "
        << (lock ? AST_DEBUG_PTR_TO_STR(lock): node.id + ANSI_RESET)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// === Primaries ===
void DebugTree::print(Primary& node) {
    os_ << "Primary_base -> " << AST_DEBUG_PTR_TO_STR(node.next)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(IdRef& node) {
    auto lock = node.ref.lock();
    os_ << ((!lock && (nextPrimary_.back() == &node ? (nextPrimary_.pop_back(), false) : true))
            ? ANSI_START ANSI_RED ANSI_AND ANSI_BOLD ANSI_APPLY : "")
        << "IdRef "
        << (lock ? AST_DEBUG_PTR_TO_STR(lock) : node.id + ANSI_RESET)
        << (node.next ? " -> " + AST_DEBUG_PTR_TO_STR(node.next) : "")
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);

    if (node.next)
        nextPrimary_.push_front(node.next.get());
}

// === Literals / Expressions ===
void DebugTree::print(BoolLiteral& node) {
    os_ << (node.val ? "true" : "false")
        << (node.debug_optimized ? ANSI_START ANSI_BOLD ANSI_AND ANSI_GREEN ANSI_APPLY + std::string(" (optimized)") + ANSI_RESET : "")
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(IntLiteral& node) {
    os_ << "int " << node.val
        << (node.debug_optimized ? ANSI_START ANSI_BOLD ANSI_AND ANSI_GREEN ANSI_APPLY + std::string(" (optimized)") + ANSI_RESET : "")
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(RealLiteral& node) {
    os_ << "real " << node.val
        << (node.debug_optimized ? ANSI_START ANSI_BOLD ANSI_AND ANSI_GREEN ANSI_APPLY + std::string(" (optimized)") + ANSI_RESET : "")
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

void DebugTree::print(BinaryExpr& node) {
    using E = ExprEnum;
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

void DebugTree::print(UnaryExpr& node) {
    using E = ExprEnum;
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
void DebugTree::print(Decl& node) {
    os_ << "decl " << node.id
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

void DebugTree::print(PrintStmt& node) {
    std::string sArgs;
    for (size_t i = 0; i < node.args.size(); ++i) {
        sArgs += AST_DEBUG_PTR_TO_STR(node.args[i]);
        if (i + 1 < node.args.size()) sArgs += ",";
    }
    os_ << "print " << sArgs
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

void DebugTree::print(IfStmt& node) {
    os_ << "if " << AST_DEBUG_PTR_TO_STR(node.condition)
        << " then " << AST_DEBUG_PTR_TO_STR(node.body);
    if (node.elseBody) os_ << " else " << AST_DEBUG_PTR_TO_STR(node.elseBody);
    os_ << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(WhileStmt& node) {
    os_ << "while " << AST_DEBUG_PTR_TO_STR(node.condition)
        << " loop " << AST_DEBUG_PTR_TO_STR(node.body)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(ForStmt& node) {
    os_ << "for " << AST_DEBUG_PTR_TO_STR(node.counter)
        << " in " << AST_DEBUG_PTR_TO_STR(node.range)
        << (node.reverse ? " reverse " : " ")
        << "loop " << AST_DEBUG_PTR_TO_STR(node.body)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(ReturnStmt& node) {
    os_ << "return " << AST_DEBUG_PTR_TO_STR(node.val)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Assignment& node) {
    os_ << AST_DEBUG_PTR_TO_STR(node.left)
        << " := " << AST_DEBUG_PTR_TO_STR(node.val)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// === Decls / Routine / Calls ===
void DebugTree::print(Var& node) {
    os_ << "var " << node.id << ": "
        << AST_DEBUG_PTR_TO_STR(node.type)
        << " is " << AST_DEBUG_PTR_TO_STR(node.val)
        << (!node.useCount ? ANSI_START ANSI_BOLD ANSI_AND ANSI_GREEN ANSI_APPLY + std::string(" (never used)") + ANSI_RESET : "")
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(Routine& node) {
    os_ << "routine " << node.id
        << " " << AST_DEBUG_PTR_TO_STR(node.type)
        << " is " << AST_DEBUG_PTR_TO_STR(node.body)
        << (!node.useCount ? ANSI_START ANSI_BOLD ANSI_AND ANSI_GREEN ANSI_APPLY + std::string(" (never used)") + ANSI_RESET : "")
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(RoutineCall& node) {
    std::string sArgs;
    for (size_t i = 0; i < node.args.size(); ++i) {
        sArgs += AST_DEBUG_PTR_TO_STR(node.args[i]);
        if (i + 1 < node.args.size()) sArgs += ",";
    }

    auto lock = node.ref.lock();
    os_ << ((!lock && (nextPrimary_.back() == &node ? (nextPrimary_.pop_back(), false) : true))
            ? ANSI_START ANSI_RED ANSI_AND ANSI_BOLD ANSI_APPLY : "")
        << "RoutineCall "
        << (lock ? AST_DEBUG_PTR_TO_STR(lock) : node.routineId+ ANSI_RESET)
        << " (" << sArgs << ")"
        << (node.next ? " -> " + AST_DEBUG_PTR_TO_STR(node.next) : "")
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);

    if (node.next)
        nextPrimary_.push_front(node.next.get());
}
void DebugTree::print(RoutineType& node) {
    std::string sParams;
    for (size_t i = 0; i < node.params.size(); ++i) {
        sParams += AST_DEBUG_PTR_TO_STR(node.params[i]);
        if (i + 1 < node.params.size()) sParams += ",";
    }
    os_ << "Type:routine (" << sParams << "): "
        << AST_DEBUG_PTR_TO_STR(node.retType)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

// === Array / Record ===
void DebugTree::print(ArrayType& node) {
    os_ << "array[" << AST_DEBUG_PTR_TO_STR(node.size) << "]: "
        << AST_DEBUG_PTR_TO_STR(node.elemType)
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(ArrayAccess& node) {
    os_ << "ArrayAccess [" << AST_DEBUG_PTR_TO_STR(node.val) << "]"
        << (node.next ? " -> " + AST_DEBUG_PTR_TO_STR(node.next) : "")
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);

    if (node.next)
        nextPrimary_.push_front(node.next.get());
}
void DebugTree::print(RecordType& node) {
    std::string sMembers;
    for (size_t i = 0; i < node.members.size(); ++i) {
        sMembers += AST_DEBUG_PTR_TO_STR(node.members[i]);
        if (i + 1 < node.members.size()) sMembers += ",";
    }
    os_ << "record {" << sMembers << "}"
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}
void DebugTree::print(RecordMember& node) {
    os_ << "RecordMember " << node.id
        << (node.next ? " -> " + AST_DEBUG_PTR_TO_STR(node.next) : "")
        << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);

    if (node.next)
        nextPrimary_.push_front(node.next.get());
}

#endif