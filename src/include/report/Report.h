#pragma once

#include "report/CompileMsg.h"
#include "utils/PrintingUtils.h"
#include "FileReader.h"
#include <iostream>

class Reporter final {
public:
    Reporter(std::shared_ptr<FileReader> file)
        : file_(file) {}

    void save(CompileMsg msg) {
        msgs_.emplace_back(std::move(msg));
    }

    bool hasErrors() const {
        return hasErrors_;
    }

    void report(const CompileMsg& msg) {
        unsigned long lineStart = file_->lineStarts[msg.span.line-1];
        unsigned long displayStart = ((msg.span.start < 16 || lineStart + 16 > msg.span.start) ? lineStart: msg.span.start - 16);

        if (msg.level == CompileMsg::Level::Error)
            hasErrors_ = true;

        char sourceText[56];
        int i = 0;
        int maxAmount = std::min(56, static_cast<int>(file_->size() - displayStart));
        for (size_t pos = displayStart; i < maxAmount; pos = displayStart + ++i) {
            char c = (*file_)[pos];
            // Assuming Lexer has already verified correctness of CRLFs, checking for CR is already enough
            if (c == '\n' || c == '\r') break;
            sourceText[i] = c == '\t' ? ' ' : c;
        }

        sourceText[i] = '\0';
        bool lineTooLong = i == 56;

        unsigned long maxArrowsLen = std::min(
            56 - (msg.span.start - displayStart),
            file_->lineStarts[msg.span.line] - msg.span.start-1 // clamp so that there are no more arrows than the length of this line
        );

        std::string title;
        switch (msg.level) {
        case CompileMsg::Level::Error: {
            title = ANSI_START ANSI_RED ANSI_AND ANSI_BOLD ANSI_APPLY + std::string("error") + ANSI_RESET + ": ";
            break;
        }
        case CompileMsg::Level::Warning: {
            title = ANSI_START ANSI_YELLOW ANSI_AND ANSI_BOLD ANSI_APPLY + std::string("warning") + ANSI_RESET + ": ";
            break;
        }
        }

        std::cout << ANSI_START ANSI_BOLD ANSI_APPLY << file_->fileName() << ":" << msg.span.line << ":"
            << msg.span.start - lineStart + 1 << ANSI_RESET << ": " // Display column right before the start of the erroneous token
            << title << std::move(msg.message) << "\n"
            << logger_.numberedWall(msg.span.line) << sourceText << (lineTooLong ? "..." : "") << "\n"
            << logger_.wall() << ANSI_START ANSI_RED ANSI_APPLY
            << logger_.arrows(
                msg.span.start - displayStart,
                (msg.span.end - msg.span.start > maxArrowsLen) ? maxArrowsLen : msg.span.end - msg.span.start
            ) << ANSI_RESET << "\n";
    }

    std::string substr(size_t start, size_t end, size_t maxSize = 10) {
        end = end - start; // end is now size
        bool tooLong = end > maxSize;
        end = std::min(end, maxSize);

        std::string s = file_->substr(start, end);
        if (tooLong)
            s += "...";
        return s;
    }
    
private:
    class Logger {
    public:
        std::string wall() const {
            return std::string(maxWallLength_ + 1, ' ') + " | ";
        }
        std::string numberedWall(unsigned long num) {
            std::string numStr = std::to_string(num);
            if (numStr.size() > maxWallLength_)
                maxWallLength_ = numStr.size();

            return " " + numStr + std::string(maxWallLength_ - numStr.size(), ' ') + " | ";
        }
        std::string arrows(unsigned long pos, unsigned length) const {
            return std::string(pos , ' ') + std::string(length, '^');
        }
    private:
        unsigned long maxWallLength_{2};
    } logger_;

    std::shared_ptr<FileReader> file_;
    std::vector<CompileMsg> msgs_;
    bool hasErrors_{false};
};