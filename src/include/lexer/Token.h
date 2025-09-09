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
        virtual ~BaseTk() = default;
        virtual void print(std::ostream& os) const;

        Span span;
        const TokenType type;
    };

    struct IdentifierTk : BaseTk {
        IdentifierTk() = delete;
        IdentifierTk(std::string id):
            BaseTk(TokenType::Identifier), identifier(std::move(id)) {}
        void print(std::ostream& os) const override;

        const std::string identifier;
    };

    struct IntTk : public BaseTk {
        IntTk():
            BaseTk(TokenType::Int) {}
        IntTk(long value):
            BaseTk(TokenType::Int), value(value) {}
        void print(std::ostream& os) const override;

        TokenType type{TokenType::Int};
        long value{0};
    };

    struct RealTk : public BaseTk {
        RealTk():
            BaseTk(TokenType::Real) {}
        RealTk(double value):
            BaseTk(TokenType::Real), value(value) {}
        void print(std::ostream& os) const override;

        TokenType type{TokenType::Real};
        double value{0};
    };
}

std::ostream& operator<<(std::ostream &os, const Tokens::BaseTk &o);