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

// Check that identifier was declared
void Analyzer::validate(Ast::IdRef& node) {
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

    if (!decl) {
        saveError(
            std::string("use of undeclared identifier: " ANSI_START ANSI_BOLD ANSI_APPLY) + node.id + ANSI_RESET,
            node.span
        );
        return;
    }

    decl->validate(*this);
}

void Analyzer::validate(Ast::Var& node) {
    
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
    node.body->validate(*this);
    currBlock_ = currBlock_->parent;
}

void Analyzer::validate(Ast::Block& node) {
    std::cout << "VALUDIT]\n";
    for (auto& unit: node.units) {
        unit->validate(*this);
    }
}