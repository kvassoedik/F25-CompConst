#include "lexer/Lexer.h"
#include "lexer/Token.h"
#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <math.h>
#include <sstream>

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
};

bool Lexer::openFile(char* fileName) {
    auto file = std::make_unique<std::ifstream>();
    file->open(fileName, std::ios::in);
    if (!file->is_open()) return false;

    lineNum_ = 1;
    currTkStart_ = 0;
    file_.swap(file);
    return true;
}

std::unique_ptr<Tokens::BaseTk> Lexer::nextToken(bool& ret_eof) {
    std::string acc;
    int byte;

    while ((byte = (file_->get())) != EOF) {
        unsigned char c = byte;
        std::cout << "Parsing symbol " << c << " (" << static_cast<int>(c) << ")\n";

        if (isLetter(c) || isDigit(c)) {
            acc += c;
        } else {
            if (c == '.') {
                if (acc.size() > 0) {
                    if (isDigit(acc[0])) {
                        // This is a real literal
                        acc += c;
                        continue;
                    }
                } else {
                    int byte;
                    if ((byte = file_->get()) != EOF && isDigit(static_cast<unsigned char>(byte))) {
                        // This is a real literal
                        acc += c;
                        continue;
                    } else {
                        file_->seekg(-1, std::ios::cur); // It's not a real, go back and let the dot be handled
                    }
                }
            }

            if (acc.size() > 0) {
                // Process the previously stored word first, this delimiter will be parsed later
                file_->seekg(-1, std::ios::cur);

                auto tk = getTokenFromWord(acc);
                initToken(*tk);
                return tk;
            }

            if (c == 10 || c == 13) { // Line Feed + Carriage Return
                // If it's Windows notation (CRLF), skip over both characters
                if (c == 13) {
                    if ((byte = file_->get()) == EOF) {
                        ret_eof = true;
                    } else {
                        char c2;
                        if ((c2 = static_cast<unsigned int>(byte)) != 13) {
                            throwError("Encountered Carriage Return without Line Feed");
                        }
                    }
                }

                auto tk = std::make_unique<Tokens::BaseTk>(TokenType::Space);
                initToken(*tk);
                lineNum_++;

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
            return tk;
        }
    }

    ret_eof = true;

    size_t len = acc.size();
    if (len == 0)
        return std::make_unique<Tokens::BaseTk>(TokenType::Space);
    
    auto tk = getTokenFromWord(acc);
    initToken(*tk);
    tk->span.end = tk->span.start + len-1; // tellg() will return a garbage number due to EOF, set end manually
    return tk;
}

std::vector<std::unique_ptr<Tokens::BaseTk>> Lexer::scan() {
    if (!file_)
        throwError("No opened file.");

    std::vector<std::unique_ptr<Tokens::BaseTk>> tokens;
    bool eof = false;

    do {
        auto tk = nextToken(eof);
        std::cout << "New token: " << *tk << "\n\n";
        tokens.emplace_back(std::move(tk));
    } while (!eof);

    return tokens;
}

std::unique_ptr<Tokens::BaseTk> Lexer::getTokenFromWord(std::string& word) {
    std::cout << "Parsing word: " << word << "\n";

    auto kw = KEYWORDS.find(word);
    if (kw != KEYWORDS.end()) return std::make_unique<Tokens::BaseTk>(kw->second);

    if (isDigit(word[0]) || word[0] == '.') {
        bool isReal = false;
        int mult = 0;

        for (char c: word) {
            if (isDigit(c)) {
                if (!isReal) mult++;
                continue;
            };
            if (c == '.') {
                if (isReal)
                    throwError("Real literal with multiple dots encountered");
                isReal = true;
            } else {
                throwError("Invalid number form containing a letter");
            }
        }

        // Extracting the number
        if (isReal) {
            double value = 0;
            for (char c: word) {
                if (c != '.') {
                    value += (c - 48) * pow(10, mult-1);
                    mult >>= 1;
                }
            }

            return std::make_unique<Tokens::RealTk>(value);
        } else {
            long value = 0;
            for (char c: word) {
                value += (c - 48) * pow(10, mult-1);
                mult -= 1;
            }

            return std::make_unique<Tokens::IntTk>(value);
        }
    }

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
        case ' ': return TokenType::Space;
        case '\t': return TokenType::Space;
        case ';': return TokenType::Space;
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
        case '/': return TokenType::DIVIDE;
        case '*': return TokenType::TIMES;
        case '<': return TokenType::LESS_THAN;
        case '>': return TokenType::MORE_THAN;
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
    ss << "[Lexer (line " << lineNum_ << ")]: " << std::move(reason);
    throw std::runtime_error(ss.str());
}