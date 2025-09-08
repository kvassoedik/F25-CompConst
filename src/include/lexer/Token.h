#pragma once

#include "lexer/TokenType.h"
#include <string>

namespace Tokens {
    struct Span {
        unsigned long line;
        unsigned int start, end;
    };

    struct BaseTk {
        BaseTk() = delete;
        BaseTk(TokenType type): type(type) {}

        const TokenType type;
        Span span;
    };

    struct IdentifierTk : BaseTk {
        IdentifierTk():
            BaseTk(TokenType::Identifier) {}
        IdentifierTk(std::string id):
            BaseTk(TokenType::Identifier), identifier(std::move(id)) {}

        std::string identifier;
    };

    struct IntTk : public BaseTk {
        IntTk():
            BaseTk(TokenType::Int) {}
        IntTk(long value):
            BaseTk(TokenType::Int), value(value) {}

        TokenType type{TokenType::Int};
        long value{0};
    };

    struct RealTk : public BaseTk {
        RealTk():
            BaseTk(TokenType::Real) {}
        RealTk(double value):
            BaseTk(TokenType::Real), value(value) {}

        TokenType type{TokenType::Real};
        double value{0};
    };
}

std::ostream& operator<<(std::ostream &os, const Tokens::BaseTk &o);
std::ostream& operator<<(std::ostream& os, const Tokens::IdentifierTk &o);
std::ostream& operator<<(std::ostream& os, const Tokens::IntTk &o);
std::ostream& operator<<(std::ostream& os, const Tokens::RealTk &o);