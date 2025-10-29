#pragma once

#include "lexer/Tokens.h"

struct CompileMsg final {
    enum class Level {
        Error,
        Warning,
        Appendix,
    } level;
    std::string message;
    Tokens::Span span;
};