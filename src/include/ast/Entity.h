#pragma once

#include "ast/Debug.h"
#include "lexer/Token.h"

#define AST_DEBUG_PRINT_METHOD_SIGNATURE \
void print(std::ostream& os, const std::string& newline)

#define AST_DEBUG_PRINT_METHOD_IMPL_TAIL \
"   " << this->span << "\n"

#define AST_DEBUG_PRINT_METHOD(_stmt_) \
AST_DEBUG_PRINT_METHOD_SIGNATURE override {\
    os << _stmt_ << AST_DEBUG_PRINT_METHOD_IMPL_TAIL;\
}

#define AST_DEBUG_PTR_TO_STR(_ptr_) \
(_ptr_ \
? (debugInfo.pushPrint(_ptr_->debugId),\
    std::string("^").append(ANSI_START ANSI_YELLOW ANSI_APPLY).append(std::to_string(_ptr_->debugId)).append(ANSI_RESET)) \
: ("^" ANSI_START ANSI_YELLOW ANSI_APPLY "N" ANSI_RESET))

namespace Ast {

struct Entity {
    Entity(Tokens::Span span)
        : span(span) {}
    virtual ~Entity() {}

    virtual AST_DEBUG_PRINT_METHOD_SIGNATURE {}
public:
    Tokens::Span span;

#if AST_DEBUG_ON
    unsigned long debugId{0};
#endif
};

}