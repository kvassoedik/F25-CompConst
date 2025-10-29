#pragma once

#include "lexer/Tokens.h"
#include "parser/DebugTree.h"
#include "utils/PrintingUtils.h"
#include "analyzer/Analyzer.h"

namespace Ast {

struct Entity {
    Entity(Tokens::Span span)
        : span(span) {}
    virtual ~Entity() {}

    virtual AST_VALIDATE_METHOD_SIGNATURE {}
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