#pragma once

#include "lexer/Tokens.h"
#include <fstream>
#include <memory>
#include <vector>

class FileReader final {
public:
    FileReader(const char* fileName) {
        auto file = std::make_unique<std::ifstream>();
        file->open(fileName, std::ios::in);
        if (!file->is_open())
            return;

        buf_.assign((std::istreambuf_iterator<char>(*file)),
            std::istreambuf_iterator<char>());
        fileName_ = std::string(fileName);

        lineStarts.reserve(10000);
        lineStarts.push_back(0);

        file_ = std::move(file); // doing LAST for exception safety
    }
    FileReader(FileReader& rhs) = delete;
    FileReader(FileReader&& rhs) {
        swap(std::move(rhs));
    }
    FileReader& operator=(FileReader& rhs) = delete;
    FileReader& operator=(FileReader&& rhs) {
        swap(std::move(rhs));
        return *this;
    }

    bool isOpen() const noexcept { return file_.get(); }
    const std::string& fileName() const noexcept { return fileName_; }
    const char& operator[](size_t i) const { return buf_[i]; }
    size_t size() const noexcept { return buf_.size(); } 
    const char* c_str() const noexcept { return buf_.c_str(); }
    std::string substr(std::size_t __pos = 0UL, std::size_t __n = 18446744073709551615UL) const { return buf_.substr(__pos, __n); }
    Tokens::Span eof() const noexcept {
        return Tokens::Span{
            lineStarts.size(),
            buf_.size(),
            buf_.size()+1,
        };
    }

    // Returns source code string with endlines substituted with spaces
    std::string extractSrc(size_t start, size_t end) const {
        if (start > end)
            throw std::range_error("FileReader::extractSrc: start must be >= end");
        std::string res;
        res.reserve(end - start);

        for (size_t i = 0, max = std::min(end, buf_.size()) - start + 1; i < max; ++i) {
            res += buf_[start + i];
            if (res[i] == '\n' || res[i] == '\r' || res[i] == '\t')
                res[i] = ' ';
        }
        return res;
    }
public:
    std::vector<unsigned long> lineStarts;
private:
    void swap(FileReader&& rhs) {
        std::swap(file_, rhs.file_);
        std::swap(fileName_, rhs.fileName_);
        std::swap(buf_, rhs.buf_);
    }
private:
    std::string fileName_;
    std::string buf_;
    std::unique_ptr<std::ifstream> file_{nullptr};
};