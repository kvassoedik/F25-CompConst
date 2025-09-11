#include "lexer/Lexer.h"
#include "lexer/Token.h"
#include "utils/PrintingUtils.h"
#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <cmath>
#include <sstream>

#define SAVE_EXCESSIVE_TOKENS 1

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

bool Lexer::openFile(char* fileName) {
    auto file = std::make_unique<std::ifstream>();
    file->open(fileName, std::ios::in);
    if (!file->is_open()) return false;

    pos_ = 0;
    lineNum_ = 1;
    currTkStart_ = 0;
    lineStartPos_ = 0;
    comment_.isStarted = false;
    comment_.isMultiline = false;
    file_.swap(file);
    fileName_.append(fileName);
    return true;
}

std::vector<std::unique_ptr<Tokens::BaseTk>> Lexer::scan() {
    if (!file_)
        throwError("No opened file.");

    buf_.assign((std::istreambuf_iterator<char>(*file_)),
            std::istreambuf_iterator<char>());

    std::vector<std::unique_ptr<Tokens::BaseTk>> tokens;
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
    unsigned char c;
    while (get(c)) {
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
                    pos_++;
                    continue;
                }

                unsigned char c2;
                if (!lookAhead(c2)) break; // EOF
                if ('/' != c2) {
                    pos_++;
                    continue;
                }

                if (canLog(2)) std::cout << "Closing a comment\n";
            } else {
                char endlineNum = checkEndline(c);
                if (!endlineNum) {
                    pos_++;
                    continue;
                }
            }
            comment_.isStarted = false;


            auto tk = std::make_unique<Tokens::BaseTk>(TokenType::COMMENT_BODY);
            initToken(*tk);
            return tk;
        }

        // Accumulate word
        if (isLetter(c) || isDigit(c)) {
            pos_++;
            continue;
        }

        // A dot?
        if (c == '.') {
            bool isDoubleDot = false;
            unsigned char c2;

            if (!lookAhead(c2)) break;
            if (c2 == '.') {
                isDoubleDot = true;
                if (currTkLen() == 0) {
                    // If nothing is accumulated, it's a DOUBLE_DOT token
                    pos_ += 2; // Step to the 2nd dot and past it
                    if (canLog(2)) std::cout << "Skip character: " << c2 << "\n";

                    auto tk = std::make_unique<Tokens::BaseTk>(TokenType::DOUBLE_DOT);
                    initToken(*tk);
                    return tk;
                }
            }
            // Otherwise, step back and process the previous word

            if (!isDoubleDot) {
                if (currTkLen() > 0) {
                    if (isDigit(buf_[currTkStart_]) || buf_[currTkStart_] == '.') {
                        // This is a real literal
                        // If the 1st char is also a dot, it's an ill form, but let it be processed by the getTokenFromWord()
                        pos_++;
                        continue;
                    }
                } else if (isDigit(c2)) {
                    // This is a real literal
                    pos_ += 2;
                    if (canLog(2)) std::cout << "Skip character: " << c2 << "\n";
                    continue;
                }
            }
        }

        // Flush the accumulated word
        if (currTkLen() > 0) {
            // Process the previously stored word first, this delimiter will be parsed later
            auto tk = getTokenFromWord();
            initToken(*tk);
            return tk;
        }

        // An endline?
        if (checkEndline(c, true)) {
            pos_++;
            auto tk = std::make_unique<Tokens::BaseTk>(TokenType::ENDLINE);
            initToken(*tk);
            lineNum_++;
            lineStartPos_ = pos_;
            return tk;
        }

        TokenType type = getDelimiterType(c);
        if (type == TokenType::INVALID) {
            if (pos_ + 1 <= buf_.size()) pos_++;
            throwError("Unrecognized symbol: "
                + std::string(1, c)
                + " (ASCII " + std::to_string(static_cast<int>(c)) + ")"
            );
        }

        pos_++;
        auto tk = std::make_unique<Tokens::BaseTk>(type);
        initToken(*tk);
        return tk;
    }

    ret_eof = true;
    if (canLog(1)) std::cout << "==> REACHED EOF <==\n";

    if (currTkLen() == 0) {
        auto tk = std::make_unique<Tokens::BaseTk>(TokenType::SPACE);
        initToken(*tk);
        return tk;
    }
    
    if (comment_.isStarted) {
        auto tk = std::make_unique<Tokens::BaseTk>(TokenType::COMMENT_BODY);
        initToken(*tk);
        return tk;
    } else {
        auto tk = getTokenFromWord();
        initToken(*tk);
        return tk;
    }
}

std::unique_ptr<Tokens::BaseTk> Lexer::getTokenFromWord() {
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
    return std::make_unique<Tokens::IdentifierTk>(std::string(word.data(), word.size()));
}

void Lexer::initToken(Tokens::BaseTk& tk) {
    tk.span.line = lineNum_;
    tk.span.start = currTkStart_;
    tk.span.end = pos_;
    currTkStart_ = tk.span.end;

    #if SAVE_TOKEN_STRING
    switch (tk.type) {
    case TokenType::ENDLINE: {
        tk._str = "\\n";
        break;
    }
    case TokenType::INDENT: {
        tk._str = "\\t";
        break;
    }
    default: tk._str = std::move(buf_.substr(tk.span.start, tk.span.end - tk.span.start));
    }
    #endif
}

TokenType Lexer::getDelimiterType(char c) {
    switch (c) {
        case ' ': return TokenType::SPACE;
        case '\t': return TokenType::INDENT;
        case ';': return TokenType::SEMICOLON;
        case ':': {
            unsigned char c2;
            if (lookAhead(c2))
                if ('=' == c2) {
                    pos_++;
                    return TokenType::ASSIGNMENT;
                }
            return TokenType::COLON;
        }
        case '+': return TokenType::PLUS;
        case '-': return TokenType::MINUS;
        case '=': {
            unsigned char c2;
            if (lookAhead(c2))
                if ('>' == c2) {
                    pos_++;
                    return TokenType::ROUTINE_ARROW;
                }
            return TokenType::EQUAL;
        }
        case '/': {
            unsigned char c2;
            if (lookAhead(c2)) {
                if ('*' == c2) {
                    comment_.isStarted = true;
                    comment_.isMultiline = true;
                    pos_++;
                    if (canLog(2)) std::cout << "Opening a comment\n";
                    return TokenType::COMMENT_OPEN;
                } else if ('/' == c2) {
                    comment_.isStarted = true;
                    comment_.isMultiline = false;
                    pos_++;
                    if (canLog(2)) std::cout << "Oneline comment\n";
                    return TokenType::COMMENT_ONELINE;
                } else if ('=' == c2) {
                    pos_++;
                    return TokenType::UNEQUAL;
                }
            }
            return TokenType::DIVIDE;
        }
        case '*': {
            unsigned char c2;
            if (lookAhead(c2))
                if ('/' == c2) {
                    pos_++;
                    return TokenType::COMMENT_CLOSE;
                }
            return TokenType::TIMES;
        }
        case '%': return TokenType::MODULO;
        case '<': {
            unsigned char c2;
            if (lookAhead(c2))
                if ('=' == c2) {
                    pos_++;
                    return TokenType::LESS_OR_EQUAL;
                }
            return TokenType::LESS_THAN;
        }
        case '>': {
            unsigned char c2;
            if (lookAhead(c2))
                if ('=' == c2) {
                    pos_++;
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

void Lexer::throwError(std::string reason) {
    std::ostringstream ss;

    // Output the erroneous line
    char buf[56];

    // 16 chars back, the rest is forward
    unsigned long displayStart = ((currTkStart_ < 16 || lineStartPos_ + 16 > currTkStart_) ? lineStartPos_: currTkStart_ - 16);
    file_->seekg(displayStart); 
    file_->getline(buf, 56);

    bool lineTooLong = file_->fail();
    if (lineTooLong)
        file_->clear();

    ss << ANSI_START ANSI_BOLD ANSI_APPLY << fileName_ << ":" << lineNum_ << ":"
        << currTkStart_ - lineStartPos_ + 1 << ANSI_RESET << ": " // Display column right before the start of the erroneous token
        << ANSI_START ANSI_RED ANSI_AND ANSI_BOLD ANSI_APPLY "error" ANSI_RESET ": " << std::move(reason) << "\n"
        << logger_.numberedWall(lineNum_ + 1) << buf << (lineTooLong ? "..." : "") << "\n"
        << logger_.wall() << ANSI_START ANSI_RED ANSI_APPLY
        << logger_.arrows(currTkStart_ - displayStart, (pos_ - currTkStart_ > 56) ? 56 : pos_ - currTkStart_) << ANSI_RESET;
    throw std::runtime_error(ss.str());
}

char Lexer::checkEndline(unsigned char c, bool doMove) {
    if (c == 10 || c == 13) { // Line Feed + Carriage Return
        // If it's Windows notation (CRLF), skip over both characters
        if (c == 13) {
            unsigned char c2;
            if (lookAhead(c2)) {
                if (c2 != 10) {
                    throwError("encountered Carriage Return without Line Feed");
                }
                if (doMove)
                    pos_++;
                return 2;
            }
        }
        return 1;
    }
    return 0;
}

bool Lexer::lookAhead(unsigned char &c) {
    if (pos_ + 1 > buf_.size())
        return false;
    c = buf_[pos_ + 1];
    return true;
}

bool Lexer::get(unsigned char &c) {
    if (pos_ == buf_.size())
        return false;
    c = buf_[pos_];
    return true;
}

void Lexer::move() {
    if (pos_ == buf_.size())
        return;
    pos_++;
}
