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

    std::cout << ANSI_START ANSI_RED ANSI_APPLY << newline << ANSI_RESET << prefix;
    newline_.assign(size, ' ');
    newline_[0] = '\n';
    node->print(*this);
}

// ---------- Node-specific printing methods ----------

void print(Ast::Type& node) {
    std::string output;
    switch(node.code) {
        case TypeEnum::ERROR: {output = "<error>"; break;}
        case TypeEnum::RESOLVABLE: {output = "<processed>"; break;}
        case TypeEnum::Int: {output = "integer"; break;}
        case TypeEnum::Real: {output = "real"; break;}
        case TypeEnum::Bool: {output = "boolean"; break;}
        case TypeEnum::Array: {output = "array"; break;}
        case TypeEnum::Record: {output = "record"; break;}
        default: {output = "INVALID_" + std::to_string(static_cast<int>(node.code)); break;}
    }
    os_ << "Type:" << output << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}

void DebugTree::print(Ast::Block& node) {
    std::string sUnits, sDecls, sTypes;

    for (size_t i = 0; i < node.units.size(); ++i) {
        sUnits += AST_DEBUG_PTR_TO_STR(node.units[i]);
        if (i+1 < node.units.size())
            sUnits += ",";
    }

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
    os_ << "Block {"
        << newline_ << pd << "parent " << AST_DEBUG_PTR_TO_STR(node.parent)
        << newline_ << pd << "units: " << sUnits
        << newline_ << pd << "decls: " << sDecls
        << newline_ << pd << "types: " << sTypes
        << newline_ << "}" << AST_DEBUG_PRINT_METHOD_IMPL_TAIL(node.span);
}