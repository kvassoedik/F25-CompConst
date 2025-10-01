#pragma once

#include "lexer/Token.h"
#include <vector>
#include <memory>

class TokenList {
public:
    TokenList() = default;
    TokenList(std::vector<std::shared_ptr<Tokens::BaseTk>>&& tokens)
        : tokens_(std::move(tokens)) {}
    TokenList(TokenList&) = delete;
    TokenList(TokenList&& rhs)
        : tokens_(std::move(rhs.tokens_)), pos_(rhs.pos_) {}
    TokenList& operator=(TokenList&) = delete;
    TokenList& operator=(TokenList&& rhs) {
        swap(rhs);
        return *this;
    }

    std::shared_ptr<Tokens::BaseTk> get() const noexcept {
        if (pos_ >= tokens_.size())
            return nullptr;
        return tokens_[pos_];
    };
    std::shared_ptr<Tokens::BaseTk> getMove() noexcept {
        if (pos_ >= tokens_.size())
            return nullptr;
        return tokens_[pos_++];
    }
    std::shared_ptr<Tokens::BaseTk> lookAhead(size_t distance = 1) const noexcept {
        if (pos_ + distance >= tokens_.size())
            return nullptr;
        return tokens_[pos_ + distance];
    }
    inline void move() noexcept { pos_++; };
    inline size_t pos() const noexcept { return pos_; }
private:
    void swap(TokenList& rhs) {
        tokens_.swap(rhs.tokens_);
        std::swap(pos_, rhs.pos_);
    }

    std::vector<std::shared_ptr<Tokens::BaseTk>> tokens_;
    size_t pos_ = 0;
};