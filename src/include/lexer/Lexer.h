/*
    Visit website for ascii codes of characters: https://www.ascii-code.com/
*/

#pragma once

#include "lexer/Token.h"
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
    std::unique_ptr<Tokens::BaseTk> getTokenFromWord(std::string& word);
    TokenType getDelimiterType(char c);
    void initToken(Tokens::BaseTk& tk);
    void throwError(std::string reason);
    // Returns the number of characters in the endline processed, if any
    char processEndline(unsigned char c);
    bool canLog(int verbosityLevel) const noexcept { return verbosityLevel <= logVerbosity_; }
private:
    std::vector<std::unique_ptr<Tokens::BaseTk>> tokens_;
    std::unique_ptr<std::ifstream> file_{nullptr};
    unsigned long lineNum_, currTkStart_;
    int logVerbosity_;
    struct {
        bool isStarted: 1;
        bool isMultiline: 1;
    } comment_;
};