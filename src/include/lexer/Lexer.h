/*
    Visit website for ascii codes of characters: https://www.ascii-code.com/
*/

#pragma once

#include "lexer/Token.h"
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

    static bool isDigit(unsigned char c) { return (c > 47 && c < 58);}
    static bool isLetter(unsigned char c) { return ((c > 64 && c < 91) || (c > 96 && c < 123) || c == 95);}

private:
    std::unique_ptr<Tokens::BaseTk> nextToken(bool& ret_eof);
    std::unique_ptr<Tokens::BaseTk> getTokenFromWord();
    TokenType getDelimiterType(char c);
    void initToken(Tokens::BaseTk& tk);
    void throwError(std::string reason);
    // Returns the number of characters in the endline processed, if any
    char checkEndline(unsigned char c, bool doMove = false);
    bool canLog(int verbosityLevel) const noexcept { return verbosityLevel <= logVerbosity_; }

    bool lookAhead(unsigned char& c);
    bool get(unsigned char& c);
    void move();
    inline unsigned long currTkLen() const noexcept { return pos_ - currTkStart_; }
private:
    Logger logger_;
    std::vector<std::unique_ptr<Tokens::BaseTk>> tokens_;
    std::unique_ptr<std::ifstream> file_{nullptr};
    std::string fileName_;
    std::string buf_;
    unsigned long pos_, lineNum_, lineStartPos_, currTkStart_;
    int logVerbosity_;

    struct {
        bool isStarted: 1;
        bool isMultiline: 1;
    } comment_;
};