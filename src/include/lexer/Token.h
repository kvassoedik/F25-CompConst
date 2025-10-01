#pragma once

#include "lexer/TokenType.h"
#include <string>

#define SAVE_TOKEN_STRING 1

namespace Tokens {
    struct Span {
        unsigned long line, start, end;
    };

    struct BaseTk {
        BaseTk() = delete;
        BaseTk(TokenType type): type(type) {}
        virtual ~BaseTk() = default;
        virtual void print(std::ostream& os) const;

        Span span;
        const TokenType type;
#if SAVE_TOKEN_STRING
        std::string _str;
#endif
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
            BaseTk(TokenType::IntLiteral) {}
        IntTk(long value):
            BaseTk(TokenType::IntLiteral), value(value) {}
        void print(std::ostream& os) const override;

        TokenType type{TokenType::IntLiteral};
        long value{0};
    };

    struct RealTk : public BaseTk {
        RealTk():
            BaseTk(TokenType::RealLiteral) {}
        RealTk(double value):
            BaseTk(TokenType::RealLiteral), value(value) {}
        void print(std::ostream& os) const override;

        TokenType type{TokenType::RealLiteral};
        double value{0};
    };
}

std::ostream& operator<<(std::ostream &os, const Tokens::BaseTk &o);