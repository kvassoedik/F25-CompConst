#pragma once

#include "lexer/Tokens.h"
#include "analyzer/Analyzer.h"
#include "codegen/visitor.h"
#include "utils/PrintingUtils.h"

namespace ast {

struct Entity {
    Entity(Tokens::Span span)
        : span(span) {}
    virtual ~Entity() {}

    virtual AST_VALIDATE_METHOD_SIGNATURE {}
    virtual CODEGEN_METHOD_SIGNATURE { throw std::runtime_error("codegen method called on Entity"); }
#if AST_DEBUG_ON
    virtual AST_DEBUGTREE_PRINT_METHOD_SIGNATURE = 0;
#endif
public:
    Tokens::Span span;

#if AST_DEBUG_ON
    unsigned long debugId{0};
#endif
};

}