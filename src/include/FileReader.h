#pragma once

#include <fstream>
#include <memory>

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
        file_ = std::move(file);
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

    inline bool isOpen() const noexcept { return file_.get(); }
    inline const std::string& fileName() const noexcept { return fileName_; }
    inline const char& operator[](size_t i) const { return buf_[i]; }
    inline size_t size() const noexcept { return buf_.size(); } 
    inline const char* c_str() const noexcept { return buf_.c_str(); }
    inline std::string substr(std::size_t __pos = 0UL, std::size_t __n = 18446744073709551615UL) const { return buf_.substr(__pos, __n); }
private:
    void swap(FileReader&& rhs) {
        std::swap(file_, rhs.file_);
        std::swap(fileName_, rhs.fileName_);
        std::swap(buf_, rhs.buf_);
    }
private:
    std::unique_ptr<std::ifstream> file_{nullptr};
    std::string fileName_;
    std::string buf_;
};