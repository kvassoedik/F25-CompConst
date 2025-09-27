#include "lexer/Lexer.h"
#include "lexer/Token.h"
#include "utils/PrintingUtils.h"
#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <cmath>
#include <sstream>
#include <algorithm>

#define SAVE_EXCESSIVE_TOKENS 0

static std::unordered_map<std::string_view, TokenType> KEYWORDS
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
    {"in", TokenType::In},
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

int Lexer::configure(int* argc, char** argv) {
    for (int i = 1; i < *argc-1; ++i) {
        std::string_view arg(argv[i]);

        if (arg.size() > 4 &&
            "-lx" == arg.substr(0, 3))
        {
            // Messages
            std::string_view option = arg.substr(3);
            if ('V' == option.at(0)) {
                logVerbosity_ = std::clamp(std::atoi(option.substr(1).data()), 0, 2);
            } else {
                std::cerr << "Unrecognized lx option: " << option << "\n";
                return 1;
            }
        }
    }
    return 0;
}

bool Lexer::openFile(const char* fileName) {
    auto file = std::make_unique<std::ifstream>();
    file->open(fileName, std::ios::in);
    if (!file->is_open()) return false;

    pos_ = 0;
    lineNum_ = 1;
    currTkStart_ = 0;
    lineStartPos_ = 0;
    bits_.eof = false;
    bits_.commentStarted = false;
    bits_.commentMultiline = false;
    file_.swap(file);
    fileName_.append(fileName);
    return true;
}

std::vector<std::shared_ptr<Tokens::BaseTk>> Lexer::scan() {
    if (!file_)
        throw std::runtime_error("No opened file.");

    buf_.assign((std::istreambuf_iterator<char>(*file_)),
            std::istreambuf_iterator<char>());

    std::vector<std::shared_ptr<Tokens::BaseTk>> tokens;
    LexerStatus st = LexerStatus::NONE; // LexerStatus does nothing. Could be for extensibility to Formatters, Intellisense

    do {
        auto tk = nextToken(st);
        if (canLog(1)) std::cout << "* New token: " << *tk << "\n";

        #if SAVE_EXCESSIVE_TOKENS
        tokens.emplace_back(std::move(tk));
        #else
        // Don't include spaces and comments
        if (static_cast<unsigned int>(tk->type) > 9 ||
            (static_cast<unsigned int>(tk->type) > 2 && static_cast<unsigned int>(tk->type) < 5))
        {
            tokens.emplace_back(std::move(tk));
        }
        #endif
    } while (!bits_.eof);

    return tokens;
}

bool Lexer::releaseErrors() {
    bool hasErrors = !errors_.empty();
    for (auto& err: errors_) {
        // Output the erroneous line
        // 16 chars back, the rest is forward
        unsigned long displayStart = ((err.span.start < 16 || err.lineStart + 16 > err.span.start) ? err.lineStart: err.span.start - 16);

        char buf[56];
        int i = 0;
        for (pos_ = displayStart; i < 56; pos_ = displayStart + ++i) {
            if (isEndline(buf_[pos_], false)) break;
            buf[i] = buf_[pos_];
        }

        buf[i] = '\0';
        bool lineTooLong = i == 56;

        unsigned long maxArrowsLen = 56 - (err.span.start - displayStart);
        std::cout << ANSI_START ANSI_BOLD ANSI_APPLY << fileName_ << ":" << err.span.line << ":"
            << err.span.start - err.lineStart + 1 << ANSI_RESET << ": " // Display column right before the start of the erroneous token
            << ANSI_START ANSI_RED ANSI_AND ANSI_BOLD ANSI_APPLY "error" ANSI_RESET ": " << std::move(err.message) << "\n"
            << logger_.numberedWall(err.span.line) << buf << (lineTooLong ? "..." : "") << "\n"
            << logger_.wall() << ANSI_START ANSI_RED ANSI_APPLY
            << logger_.arrows(
                err.span.start - displayStart,
                (err.span.end - err.span.start > maxArrowsLen) ? maxArrowsLen : err.span.end - err.span.start
            ) << ANSI_RESET << "\n";
    }

    errors_.clear();
    return hasErrors;
}

std::shared_ptr<Tokens::BaseTk> Lexer::nextToken(LexerStatus& ret_st) {
    char c;
    while (get(c)) {
        if (c < 0) {
            move(1);
            saveError(LexerStatus::NON_ASCII, "non-ASCII character encountered (ASCII "
                + std::to_string(static_cast<int>(static_cast<unsigned char>(c))) + ")");
            pos_--;

            // Immediatelly break, as multi-byte characters break the position accounting (one character consumes several steps)
            break;
        }
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
            case '\r': {
                std::cout << "Got character: \r\n";
                break;
            }
            default: std::cout << "Got character: " << c << "\n";
            }
        }

        // Check if currently inside a comment
        if (bits_.commentStarted) {
            if (bits_.commentMultiline) {
                if (c != '*') {
                    move(1);
                    continue;
                }

                char c2;
                if (!lookAhead(c2)) break; // EOF
                if ('/' != c2) {
                    move(1);
                    continue;
                }

                if (canLog(2)) {
                    std::cout << "Skip character: " << c2 << "\n";
                    std::cout << "Closing a comment\n";
                }
            } else {
                if (!isEndline(c, false)) {
                    move(1);
                    continue;
                }
            }
            bits_.commentStarted = false;

            auto tk = std::make_shared<Tokens::BaseTk>(TokenType::COMMENT_BODY); initToken(tk);
            return tk;
        }

        // Accumulate word
        if (isLetter(c) || isDigit(c)) {
            move(1);
            continue;
        }

        // A dot?
        if (c == '.') {
            bool isDoubleDot = false;
            char c2;

            if (!lookAhead(c2)) break;
            if (c2 == '.') {
                isDoubleDot = true;
                if (currTkLen() == 0) {
                    // If nothing is accumulated, it's a DOUBLE_DOT token
                    move(2); // Step to the 2nd dot and past it
                    if (canLog(2)) std::cout << "Skip character: " << c2 << "\n";

                    auto tk = std::make_shared<Tokens::BaseTk>(TokenType::DOUBLE_DOT); initToken(tk);
                    return tk;
                }
            }
            // Otherwise, step back and process the previous word

            if (!isDoubleDot) {
                if (currTkLen() > 0) {
                    if (isDigit(buf_[currTkStart_]) || buf_[currTkStart_] == '.') {
                        // This is a real literal
                        // If the 1st char is also a dot, it's an ill form, but let it be processed by the getTokenFromWord()
                        move(1);
                        continue;
                    }
                } else if (isDigit(c2)) {
                    // This is a real literal
                    move(2); // step to the digit and past it
                    if (canLog(2)) std::cout << "Skip character: " << c2 << "\n";
                    continue;
                }
            }
        }

        // Flush the accumulated word
        if (currTkLen() > 0) {
            // Process the previously stored word first, this delimiter will be parsed later
            auto tk = getTokenFromWord(ret_st); initToken(tk);
            return tk;
        }

        // An endline?
        if (isEndline(c, true)) {
            move(1);
            auto tk = std::make_shared<Tokens::BaseTk>(TokenType::ENDLINE); initToken(tk);
            lineNum_++;
            lineStartPos_ = pos_;
            return tk;
        }

        TokenType type = getDelimiterType(c);
        move(1);

        if (type == TokenType::INVALID) {
            saveError(LexerStatus::UNSUPPORTED_SYMBOL, "Unsupported symbol: "
                + std::string{c}
                + " (ASCII " + std::to_string(static_cast<int>(static_cast<unsigned char>(c))) + ")"
            );
        }

        auto tk = std::make_shared<Tokens::BaseTk>(type); initToken(tk);
        return tk;
    }

    if (canLog(1)) std::cout << "==> REACHED EOF <==\n";
    if (currTkLen() == 0) {
        bits_.eof = true;
        auto tk = std::make_shared<Tokens::BaseTk>(TokenType::END_OF_FILE); initToken(tk);
        return tk;
    }
    
    if (bits_.commentStarted) {
        auto tk = std::make_shared<Tokens::BaseTk>(TokenType::COMMENT_BODY); initToken(tk);
        return tk;
    } else {
        TokenType tt = getDelimiterType(c);
        if (tt != TokenType::INVALID) {
            auto tk = std::make_shared<Tokens::BaseTk>(tt); initToken(tk);
            return tk;
        }
        auto tk = getTokenFromWord(ret_st); initToken(tk);
        return tk;
    }
}

std::shared_ptr<Tokens::BaseTk> Lexer::getTokenFromWord(LexerStatus& ret_st) {
    std::string_view word(buf_.c_str() + currTkStart_, pos_ - currTkStart_);
    if (word.empty())
        throw std::runtime_error("cannot lex an empty word");
    if (canLog(1)) std::cout << "Lexing word: " << word << "\n";

    // If found in keyword map, it's a Keyword
    auto kw = KEYWORDS.find(word);
    if (kw != KEYWORDS.end()) return std::make_unique<Tokens::BaseTk>(kw->second);

    // If has a digit or dot at the front, it's a number
    if (isDigit(word[0]) || word[0] == '.') {
        bool isReal = false;

        for (char c: word) {
            if (c == '.') {
                if (isReal) {
                    saveError(LexerStatus::REAL_MANY_DOTS, "real literal with multiple dots encountered");
                    return std::make_unique<Tokens::BaseTk>(TokenType::INVALID);
                }
                isReal = true;
            } else if (!isDigit(c)) {
                saveError(LexerStatus::NUMBER_WITH_LETTER, "invalid number form containing a letter");
                return std::make_unique<Tokens::BaseTk>(TokenType::INVALID);
            }
        }

        // Extracting the number
        if (isReal) {
            double value = 0;
            size_t dotPos;

            for (dotPos = 0; dotPos < word.size(); ++dotPos) {
                char c = word[dotPos];
                if (c == '.') break;

                char digit = c - '0';
                if (value > std::numeric_limits<double>::max() - digit
                    || value + digit > std::numeric_limits<double>::max() / 10) {
                        saveError(LexerStatus::REAL_EXCEED, "real literal exceeded max limit");
                        return std::make_unique<Tokens::BaseTk>(TokenType::INVALID);
                    }

                value = value * 10 + digit;
            }

            double fraction = 0;
            for (size_t i = word.size()-1; i > dotPos; --i) {
                fraction = (fraction + (word[i] - '0')) * 0.1;
            }

            if (value > std::numeric_limits<double>::max() - fraction) {
                saveError(LexerStatus::REAL_EXCEED, "real literal exceeded max limit with fraction");
                return std::make_unique<Tokens::BaseTk>(TokenType::INVALID);
            }

            return std::make_unique<Tokens::RealTk>(value + fraction);
        } else {
            long value = 0;
            
            for (char c: word) {
                char digit = c - '0';

                if (value > std::numeric_limits<long>::max() - digit
                    || value + digit > std::numeric_limits<long>::max() / 10 + 7) {
                        saveError(LexerStatus::INT_EXCEED, "integer literal exceeded max limit");
                        return std::make_unique<Tokens::BaseTk>(TokenType::INVALID);
                    }

                value = value * 10 + digit;
            }

            return std::make_unique<Tokens::IntTk>(value);
        }
    }

    // Otherwise, its an identifier
    return std::make_unique<Tokens::IdentifierTk>(std::string(word.data(), word.size()));
}

void Lexer::initToken(std::shared_ptr<Tokens::BaseTk>& tk) {
    tk->span.line = lineNum_;
    tk->span.start = currTkStart_;
    tk->span.end = pos_;
    currTkStart_ = tk->span.end;

    #if SAVE_TOKEN_STRING
    switch (tk->type) {
    case TokenType::ENDLINE: {
        tk->_str = "\\n";
        break;
    }
    case TokenType::INDENT: {
        tk->_str = "\\t";
        break;
    }
    default: tk->_str = std::move(buf_.substr(tk->span.start, tk->span.end - tk->span.start));
    }
    #endif
}

TokenType Lexer::getDelimiterType(char c) {
    switch (c) {
        case ' ': return TokenType::SPACE;
        case '\t': return TokenType::INDENT;
        case ';': return TokenType::SEMICOLON;
        case ':': {
            char c2;
            if (lookAhead(c2))
                if ('=' == c2) {
                    move(1);
                    if (canLog(2)) std::cout << "Skip character: " << c2 << "\n";
                    return TokenType::ASSIGNMENT;
                }
            return TokenType::COLON;
        }
        case '+': return TokenType::PLUS;
        case '-': return TokenType::MINUS;
        case '=': {
            char c2;
            if (lookAhead(c2))
                if ('>' == c2) {
                    move(1);
                    if (canLog(2)) std::cout << "Skip character: " << c2 << "\n";
                    return TokenType::ROUTINE_ARROW;
                }
            return TokenType::EQUAL;
        }
        case '/': {
            char c2;
            if (lookAhead(c2)) {
                if ('*' == c2) {
                    bits_.commentStarted = true;
                    bits_.commentMultiline = true;
                    move(1);
                    if (canLog(2)) {
                        std::cout << "Skip character: " << c2 << "\n";
                        std::cout << "Opening a comment\n";
                    }
                    return TokenType::COMMENT_OPEN;
                } else if ('/' == c2) {
                    bits_.commentStarted = true;
                    bits_.commentMultiline = false;
                    move(1);
                    if (canLog(2)) {
                        std::cout << "Skip character: " << c2 << "\n";
                        std::cout << "Oneline comment\n";
                    }
                    return TokenType::COMMENT_ONELINE;
                } else if ('=' == c2) {
                    move(1);
                    if (canLog(2)) std::cout << "Skip character: " << c2 << "\n";
                    return TokenType::UNEQUAL;
                }
            }
            return TokenType::DIVIDE;
        }
        case '*': {
            char c2;
            if (lookAhead(c2))
                if ('/' == c2) {
                    move(1);
                    if (canLog(2)) std::cout << "Skip character: " << c2 << "\n";
                    return TokenType::COMMENT_CLOSE;
                }
            return TokenType::TIMES;
        }
        case '%': return TokenType::MODULO;
        case '<': {
            char c2;
            if (lookAhead(c2))
                if ('=' == c2) {
                    move(1);
                    if (canLog(2)) std::cout << "Skip character: " << c2 << "\n";
                    return TokenType::LESS_OR_EQUAL;
                }
            return TokenType::LESS_THAN;
        }
        case '>': {
            char c2;
            if (lookAhead(c2))
                if ('=' == c2) {
                    move(1);
                    if (canLog(2)) std::cout << "Skip character: " << c2 << "\n";
                    return TokenType::MORE_OR_EQUAL;
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

bool Lexer::isEndline(char c, bool doMove) {
    if (c == 10 || c == 13) { // Line Feed + Carriage Return
        // If it's Windows notation (CRLF), skip over both characters
        if (c == 13) {
            char c2;
            if (lookAhead(c2)) {
                if (c2 != 10) {
                    saveError(LexerStatus::CR_NO_LF, "encountered Carriage Return without Line Feed");
                    bits_.eof = true;
                    return 1;
                }
                if (doMove)
                    move(1);
            }
        }
        return true;
    }
    return false;
}

bool Lexer::lookAhead(char &c) const noexcept {
    if (pos_ + 1 > buf_.size())
        return false;
    c = buf_[pos_ + 1];
    return true;
}

bool Lexer::get(char &c) const noexcept {
    if (pos_ >= buf_.size())
        return false;
    c = buf_[pos_];
    return true;
}

void Lexer::move(unsigned long step) noexcept {
    pos_ += step;
}

void Lexer::saveError(LexerStatus st, std::string reason) {
    LexerError err;
    err.message = std::move(reason);
    err.span.line = lineNum_;
    err.span.start = currTkStart_;
    err.span.end = pos_;
    err.lineStart = lineStartPos_;
    errors_.push_back(err);
}