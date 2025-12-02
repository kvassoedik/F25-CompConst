/*
    Visit website for ascii codes of characters: https://www.ascii-code.com/
*/

#pragma once

#include "utils/report/Report.h"
#include "utils/FileReader.h"
#include <vector>

class Lexer {
public:
    Lexer(std::shared_ptr<FileReader> file)
        : file_(std::move(file)) {}

    int configure(int* argc, char** argv);
    std::vector<std::shared_ptr<Tokens::BaseTk>> run();
    bool hasErrors() const { return reporter_.hasErrors(); };

    static bool isDigit(char c) { return (c > 47 && c < 58);}
    static bool isLetter(char c) { return ((c > 64 && c < 91) || (c > 96 && c < 123) || c == 95);}

private:
    std::shared_ptr<Tokens::BaseTk> nextToken();
    std::shared_ptr<Tokens::BaseTk> getTokenFromWord();
    void initToken(std::shared_ptr<Tokens::BaseTk>& tk);
    TokenType getDelimiterType(char c);

    bool isEndline(char c, bool doMove); // Returns the number of characters in the endline processed, if any
    bool lookAhead(char& c) const noexcept;
    bool get(char& c) const noexcept;
    void move(unsigned long step) noexcept;
    unsigned long currTkLen() const noexcept { return pos_ - currTkStart_; }

    bool canLog(int verbosityLevel) const noexcept { return verbosityLevel <= config_.logVerbosity; }
    void saveError(std::string reason);
private:
    std::shared_ptr<FileReader> file_;
    Reporter reporter_{file_};
    std::vector<CompileMsg> msgs_;
    unsigned long pos_{0}, lineNum_{1}, currTkStart_{0};

    struct {
        int logVerbosity{0};
    } config_;

    struct {
        bool eof = false;
        bool commentStarted = false;
        bool commentMultiline = false;
        bool waitingForMultilineCommentClose = false;
    } flags_;
};