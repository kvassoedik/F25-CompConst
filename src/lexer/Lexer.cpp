#include "lexer/Lexer.h"
#include "lexer/Token.h"
#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <cmath>
#include <sstream>

#define SAVE_EXCESSIVE_TOKENS 1

static std::unordered_map<std::string, TokenType> KEYWORDS
{
    {"true", TokenType::True},
    {"false", TokenType::False},
    {"routine", TokenType::Routine},
    {"is", TokenType::Is},
    {"print", TokenType::Print},
    {"var", TokenType::Var},
    {"if", TokenType::If},
    {"else", TokenType::Else},
    {"while", TokenType::While},
    {"for", TokenType::For},
    {"loop", TokenType::Loop},
    {"reverse", TokenType::Reverse},
    {"and", TokenType::And},
    {"or", TokenType::Or},
    {"xor", TokenType::Xor},
    {"array", TokenType::Array},
    {"record", TokenType::Record},
    {"end", TokenType::End},
    {"integer", TokenType::IntegerType},
    {"real", TokenType::RealType},
    {"boolean", TokenType::BooleanType},
};

bool Lexer::openFile(char* fileName) {
    auto file = std::make_unique<std::ifstream>();
    file->open(fileName, std::ios::in);
    if (!file->is_open()) return false;

    lineNum_ = 1;
    currTkStart_ = 0;
    posInLine_ = -1;
    comment_.isStarted = false;
    comment_.isMultiline = false;
    file_.swap(file);
    fileName_.append(fileName);
    return true;
}

std::vector<std::unique_ptr<Tokens::BaseTk>> Lexer::scan() {
    if (!file_)
        throwError("No opened file.");

    std::vector<std::unique_ptr<Tokens::BaseTk>> tokens;
    acc_.reserve(8 * 1024);
    bool eof = false;

    do {
        auto tk = nextToken(eof);
        if (canLog(1)) std::cout << "* New token: " << *tk << "\n";

        #if SAVE_EXCESSIVE_TOKENS
        tokens.emplace_back(std::move(tk));
        #else
        // Don't include spaces and comments
        if (static_cast<unsigned int>(tk->type) > 8 ||
            (static_cast<unsigned int>(tk->type) > 2 && static_cast<unsigned int>(tk->type) < 5))
        {
            tokens.emplace_back(std::move(tk));
        }
        #endif
    } while (!eof);

    return tokens;
}

std::unique_ptr<Tokens::BaseTk> Lexer::nextToken(bool& ret_eof) {
    int byte;

    while ((byte = (file_->get())) != EOF) {
        unsigned char c = byte;
        posInLine_++;

        if (canLog(2)) {
            switch (c)
            {
            case '\n': {
                std::cout << "Got character: \\n\n";
                break;
            }
            case '\t': {
                std::cout << "Got character: \\t\n";
                break;
            }
            case '\r':
                std::cout << "Got character: \r\n";
            default: std::cout << "Got character: " << c << "\n";
            }
        }

        // Check if currently inside a comment
        if (comment_.isStarted) {
            if (comment_.isMultiline) {
                if (c != '*') {
                    // Accumulate comment
                    accLen_++;

                    #if SAVE_TOKEN_STRING
                    acc_ += c;
                    #endif
                    continue;
                }

                if ((byte = file_->get()) == EOF) break;
                if ('/' != static_cast<unsigned char>(byte)) {
                    // Accumulate comment
                    accLen_ += 2;

                    #if SAVE_TOKEN_STRING
                    acc_ += c;
                    acc_ += static_cast<unsigned char>(byte);
                    #endif
                    continue;
                }

                comment_.isStarted = false;
                if (canLog(2)) std::cout << "Closing a comment\n";

                file_->seekg(-2, std::ios::cur); // little hardcode for "*/" token's length
            } else {
                char endlineNum = processEndline(c);
                if (!endlineNum) {
                    // Accumulate comment
                    accLen_++;
                    
                    #if SAVE_TOKEN_STRING
                    acc_ += c;
                    #endif
                    continue;
                }
                // Endline has been hit, seek back to process the endline again after
                file_->seekg(-endlineNum, std::ios::cur);
                comment_.isStarted = false;
            }
            
            auto tk = std::make_unique<Tokens::BaseTk>(TokenType::COMMENT_BODY);
            #if SAVE_TOKEN_STRING
            tk->_str = std::move(acc_);
            accLen_ = 0;
            #endif
            initToken(*tk);
            return tk;
        }

        if (isLetter(c) || isDigit(c)) {
            acc_ += c;
            accLen_++;
        } else {
            if (c == '.') {
                if (acc_.size() > 0) {
                    if (isDigit(acc_[0])) {
                        // This is a real literal
                        acc_ += c;
                        accLen_++;
                        continue;
                    }
                } else {
                    int byte;
                    if ((byte = file_->get()) != EOF && isDigit(static_cast<unsigned char>(byte))) {
                        // This is a real literal. Add both this dot and the following char and move on
                        acc_ += c;
                        acc_ += static_cast<unsigned char>(byte);
                        accLen_ += 2;
                        continue;
                    } else {
                        file_->seekg(-1, std::ios::cur); // It's not a real, go back and let the dot be handled
                    }
                }
            }

            if (accLen_ > 0) {
                // Process the previously stored word first, this delimiter will be parsed later
                file_->seekg(-1, std::ios::cur);

                auto tk = getTokenFromWord(acc_);
                initToken(*tk);

                #if SAVE_TOKEN_STRING
                if (tk->type != TokenType::Identifier && tk->type != TokenType::IntLiteral
                    && tk->type != TokenType::RealLiteral && tk->type != TokenType::BoolLiteral) {
                    tk->_str = std::move(acc_);
                }
                #endif

                acc_.clear();
                accLen_ = 0;
                return tk;
            }

            if (processEndline(c)) {
                auto tk = std::make_unique<Tokens::BaseTk>(TokenType::ENDLINE);
                initToken(*tk);
                lineNum_++;
                posInLine_ = -1;
                #if SAVE_TOKEN_STRING
                tk->_str = "\\n";
                #endif
                return tk;
            }

            TokenType type = getDelimiterType(c);
            if (type == TokenType::INVALID)
                throwError("Unrecognized symbol: "
                    + std::string(1, c)
                    + " (ASCII: " + std::to_string(static_cast<int>(c)) + ")"
                );

            auto tk = std::make_unique<Tokens::BaseTk>(type);
            initToken(*tk);
            #if SAVE_TOKEN_STRING
            // Band-aid for displaying delimiter tokens with length over 1 character
            switch (tk->type) {
            case TokenType::INDENT: {
                tk->_str = "\\t";
                break;
            }
            case TokenType::COMMENT_ONELINE: {
                tk->_str = "//";
                break;
            }
            case TokenType::COMMENT_OPEN: {
                tk->_str = "/*";
                break;
            }
            case TokenType::COMMENT_CLOSE: {
                tk->_str = "*/";
                break;
            }
            case TokenType::ASSIGNMENT: {
                tk->_str = ":=";
                break;
            }
            case TokenType::UNEQUAL: {
                tk->_str = "/=";
                break;
            }
            case TokenType::MORE_OR_EQUAL: {
                tk->_str = ">=";
                break;
            }
            case TokenType::LESS_OR_EQUAL: {
                tk->_str = "<=";
                break;
            }
            default: tk->_str = c;
            }
            #endif
            return tk;
        }
    }

    ret_eof = true;
    if (canLog(1)) std::cout << "==> REACHED EOF <==\n";

    if (accLen_ == 0)
        return std::make_unique<Tokens::BaseTk>(TokenType::SPACE);
    
    if (comment_.isStarted) {
        auto tk = std::make_unique<Tokens::BaseTk>(TokenType::COMMENT_BODY);
        #if SAVE_TOKEN_STRING
        tk->_str = std::move(acc_);
        accLen_ = 0;
        #endif
        initToken(*tk);
        return tk;
    } else {
        auto tk = getTokenFromWord(acc_);
        initToken(*tk);

        #if SAVE_TOKEN_STRING
        if (tk->type != TokenType::Identifier) {
            tk->_str = std::move(acc_);
            accLen_ = 0;
        }
        #endif

        tk->span.end = tk->span.start + accLen_-1; // tellg() will return a garbage number due to EOF, set end manually
        return tk;
    }
}

std::unique_ptr<Tokens::BaseTk> Lexer::getTokenFromWord(std::string& word) {
    if (word.empty())
        throw std::runtime_error("cannot lex an empty word");
    if (canLog(1)) std::cout << "Lexing word: " << word << "\n";

    // If found in keyword map, it's a Keyword
    auto kw = KEYWORDS.find(word);
    if (kw != KEYWORDS.end()) return std::make_unique<Tokens::BaseTk>(kw->second);

    // If has a digit or dot at the front, it's a number
    if (isDigit(word[0]) || word[0] == '.') {
        bool isReal = false;

        for (unsigned char c: word) {
            if (c == '.') {
                if (isReal)
                    throwError("real literal with multiple dots encountered");
                isReal = true;
            } else if (!isDigit(c)) {
                throwError("invalid number form containing a letter");
            }
        }

        // Extracting the number
        if (isReal) {
            double value = 0;
            size_t dotPos;

            for (dotPos = 0; dotPos < word.size(); ++dotPos) {
                unsigned char c = word[dotPos];
                if (c == '.') break;

                unsigned char digit = c - '0';
                if (value > std::numeric_limits<double>::max() - digit
                    || value + digit > std::numeric_limits<double>::max() / 10)
                    throwError("real literal exceeded max limit");

                value = value * 10 + digit;
            }

            double residue = 0;
            for (size_t i = word.size()-1; i > dotPos; --i) {
                residue = (residue + (word[i] - '0'))/10.0;
            }

            if (value > std::numeric_limits<double>::max() - residue)
                throwError("real literal exceeded max limit when applying denorm");

            return std::make_unique<Tokens::RealTk>(value + residue);
        } else {
            long value = 0;
            
            for (unsigned char c: word) {
                unsigned char digit = c - '0';

                if (value > std::numeric_limits<long>::max() - digit
                    || value + digit > std::numeric_limits<long>::max() / 10 + 7)
                    throwError("integer literal exceeded max limit");

                value = value * 10 + digit;
            }

            return std::make_unique<Tokens::IntTk>(value);
        }
    }

    // Otherwise, its an identifier
    return std::make_unique<Tokens::IdentifierTk>(std::move(word));
}

void Lexer::initToken(Tokens::BaseTk& tk) {
    tk.span.line = lineNum_;
    tk.span.start = currTkStart_;
    tk.span.end = file_->tellg();
    currTkStart_ = tk.span.end + 1;
}

TokenType Lexer::getDelimiterType(char c) {
    switch (c) {
        case ' ': return TokenType::SPACE;
        case '\t': return TokenType::INDENT;
        case ';': return TokenType::SEMICOLON;
        case ':': {
            int byte;
            if ((byte = file_->get()) != EOF) {
                if ('=' == static_cast<unsigned char>(byte)) return TokenType::ASSIGNMENT;
                file_->seekg(-1, std::ios::cur); // oh, you're not ":=", sorry for the trouble, going back :P
            }
            return TokenType::COLON;
        }
        case '+': return TokenType::PLUS;
        case '-': return TokenType::MINUS;
        case '=': {
            int byte;
            if ((byte = file_->get()) != EOF) {
                if ('>' == static_cast<unsigned char>(byte)) return TokenType::ROUTINE_ARROW;
                file_->seekg(-1, std::ios::cur);
            }
            return TokenType::EQUAL;
        }
        case '/': {
            int byte;
            if ((byte = file_->get()) != EOF) {
                if ('*' == static_cast<unsigned char>(byte)) {
                    comment_.isStarted = true;
                    comment_.isMultiline = true;
                    if (canLog(2)) std::cout << "Opening a comment\n";
                    return TokenType::COMMENT_OPEN;
                } else if ('/' == static_cast<unsigned char>(byte)) {
                    comment_.isStarted = true;
                    comment_.isMultiline = false;
                    if (canLog(2)) std::cout << "Oneline comment\n";
                    return TokenType::COMMENT_ONELINE;
                } else if ('=' == static_cast<unsigned char>(byte)) {
                    return TokenType::UNEQUAL;
                }
                file_->seekg(-1, std::ios::cur);
            }
            return TokenType::DIVIDE;
        }
        case '*': {
            int byte;
            if ((byte = file_->get()) != EOF) {
                if ('/' == static_cast<unsigned char>(byte)) return TokenType::COMMENT_CLOSE;
                file_->seekg(-1, std::ios::cur);
            }
            return TokenType::TIMES;
        }
        case '%': return TokenType::MODULO;
        case '<': {
            int byte;
            if ((byte = file_->get()) != EOF) {
                if ('=' == static_cast<unsigned char>(byte)) return TokenType::LESS_OR_EQUAL;
                file_->seekg(-1, std::ios::cur);
            }
            return TokenType::LESS_THAN;
        }
        case '>': {
            int byte;
            if ((byte = file_->get()) != EOF) {
                if ('=' == static_cast<unsigned char>(byte)) return TokenType::MORE_OR_EQUAL;
                file_->seekg(-1, std::ios::cur);
            }
            return TokenType::MORE_THAN;
        }
        case '.': return TokenType::DOT;
        case ',': return TokenType::COMMA;
        case '(': return TokenType::BRACKET_OPEN;
        case ')': return TokenType::BRACKET_CLOSE;
        case '[': return TokenType::SQUARE_BRACKET_OPEN;
        case ']': return TokenType::SQUARE_BRACKET_CLOSE;
        default: return TokenType::INVALID;
    }
}

void Lexer::throwError(std::string reason) {
    std::ostringstream ss;
    ss << fileName_ << ":" << getLine() << ":" << getCol() << ": " << std::move(reason);
    throw std::runtime_error(ss.str());
}

char Lexer::processEndline(unsigned char c) {
    if (c == 10 || c == 13) { // Line Feed + Carriage Return
        // If it's Windows notation (CRLF), skip over both characters
        if (c == 13) {
            int byte;
            if ((byte = file_->get()) != EOF) {
                char c2;
                if ((c2 = static_cast<unsigned int>(byte)) != 13) {
                    throwError("Encountered Carriage Return without Line Feed");
                }
                return 2;
            }
        }
        return 1;
    }
    return 0;
}