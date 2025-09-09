#include "lexer/Token.h"
#include <sstream>

std::ostream& operator<<(std::ostream &os, const Tokens::BaseTk& o) {
    o.print(os);
    return os;
}

void Tokens::BaseTk::print(std::ostream& os) const {
    os << "BaseTk{ " << static_cast<unsigned int>(type)
        << " (l" << span.line << " " << span.start << ":" << span.end << ") }";
}

void Tokens::IdentifierTk::print(std::ostream& os) const {
    os << "IdentifierTk{ " << identifier
        << " (l" << span.line << " " << span.start << ":" << span.end << ") }";
}

void Tokens::IntTk::print(std::ostream& os) const {
    os << "IdentifierTk{ " << value
        << " (l" << span.line << " " << span.start << ":" << span.end << ") }";
}

void Tokens::RealTk::print(std::ostream& os) const {
    os << "IdentifierTk{ " << value
        << " (l" << span.line << " " << span.start << ":" << span.end << ") }";
}