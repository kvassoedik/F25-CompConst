#pragma once

#include "lexer/Token.h"
#include <fstream>
#include <vector>
#include <memory>

class Lexer {
public:
    bool openFile(char* fileName);
    std::vector<Tokens::BaseTk> scan();

    static bool isDigit(char c) { return (c > 47 && c < 58);}
    static bool isLetter(char c) { return ((c > 64 && c < 91) || (c > 96 && c < 123));}

private:
    Tokens::BaseTk nextToken(bool& ret_eof);
    Tokens::BaseTk getTokenFromWord(const std::string& word);
    TokenType getDelimiterType(char c);
    void initToken(Tokens::BaseTk& tk);
    void throwError(std::string reason);
private:
    std::unique_ptr<std::ifstream> file_{nullptr};
    unsigned long lineNum_, lineStartPos_, currTkStart_;
    size_t posInLine_;
};