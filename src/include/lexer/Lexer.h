/*
    Visit website for ascii codes of characters: https://www.ascii-code.com/
*/

#pragma once

#include "lexer/Token.h"
#include "lexer/LexerStatus.h"
#include "utils/Logger.h"
#include <fstream>
#include <vector>
#include <memory>

class Lexer {
public:
    Lexer(int logVerbosity):
        logVerbosity_(logVerbosity) {}

    bool openFile(char* fileName);
    std::vector<std::unique_ptr<Tokens::BaseTk>> scan();
    bool releaseErrors();

    static bool isDigit(char c) { return (c > 47 && c < 58);}
    static bool isLetter(char c) { return ((c > 64 && c < 91) || (c > 96 && c < 123) || c == 95);}

private:
    std::unique_ptr<Tokens::BaseTk> nextToken(LexerStatus& ret_st);
    std::unique_ptr<Tokens::BaseTk> getTokenFromWord(LexerStatus& ret_st);
    std::unique_ptr<Tokens::BaseTk> initToken(std::unique_ptr<Tokens::BaseTk> tk);
    TokenType getDelimiterType(char c);

    bool isEndline(char c, bool doMove); // Returns the number of characters in the endline processed, if any
    bool lookAhead(char& c) const noexcept;
    bool get(char& c) const noexcept;
    inline void move(unsigned long step) noexcept;
    inline unsigned long currTkLen() const noexcept { return pos_ - currTkStart_; }

    bool canLog(int verbosityLevel) const noexcept { return verbosityLevel <= logVerbosity_; }
    void saveError(LexerStatus st, std::string reason);
private:
    struct LexerError {
        std::string message;
        Tokens::Span span;
        unsigned long lineStart;
    };

    Logger logger_;
    std::vector<std::unique_ptr<Tokens::BaseTk>> tokens_;
    std::vector<LexerError> errors_;
    std::unique_ptr<std::ifstream> file_{nullptr};
    std::string fileName_;
    std::string buf_;
    unsigned long pos_, lineNum_, lineStartPos_, currTkStart_;
    int logVerbosity_;

    struct {
        bool eof: 1;
        bool commentStarted: 1;
        bool commentMultiline: 1;
    } bits_;
};