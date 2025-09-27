#pragma once

#include "lexer/Token.h"
#include <vector>
#include <memory>

class TokenList {
public:
    TokenList() = delete;
    TokenList(std::vector<std::shared_ptr<Tokens::BaseTk>>&& tokens)
        : tokens_(std::move(tokens)) {}
    TokenList(TokenList&) = delete;
    TokenList(TokenList&& rhs)
        : tokens_(std::move(rhs.tokens_)), pos_(rhs.pos_) {}
    TokenList& operator=(TokenList&) = delete;
    TokenList& operator=(TokenList&& rhs) = delete;
    // {
    //     swap(rhs);
    //     return *this;
    // }

    std::pair<bool, std::shared_ptr<Tokens::BaseTk>> get() const noexcept {
        if (pos_ == tokens_.size())
            return {false, nullptr};
        return {true, tokens_[pos_]};
    };
    std::pair<bool, std::shared_ptr<Tokens::BaseTk>> lookAhead(size_t distance = 1) const noexcept {
        if (pos_ + distance >= tokens_.size())
            return {false, nullptr};
        return {true, tokens_[pos_ + distance]};
    }
    void move() noexcept {
        if (pos_ < tokens_.size())
            pos_++;
    };
private:
    void swap(TokenList& rhs) {
        tokens_.swap(rhs.tokens_);
        std::swap(pos_, rhs.pos_);
    }

    std::vector<std::shared_ptr<Tokens::BaseTk>> tokens_;
    size_t pos_ = 0;
};