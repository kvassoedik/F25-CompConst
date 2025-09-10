#pragma once

#include <iostream>

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
};