#pragma once

#include "lexer/Token.h"

struct CompileMsg final {
    enum class Level {
        Error,
        Warning,
        Appendix,
    } level;
    std::string message;
    Tokens::Span span;
};