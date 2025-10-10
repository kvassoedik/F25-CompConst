#include "lexer/Token.h"
#include <sstream>

std::ostream& operator<<(std::ostream& os, const Tokens::BaseTk& o) {
    o.print(os);
    return os;
}

std::ostream& operator<<(std::ostream& os, const Tokens::Span& o) {
    return os << "@" << o.line << ":" << o.start << ":" << o.end;
}

void Tokens::BaseTk::print(std::ostream& os) const {
    os << "BaseTk{ "
#if SAVE_TOKEN_STRING
        << _str << "   "
#endif
        << static_cast<unsigned int>(type)
        << " (_" << span.line << " " << span.start << ":" << span.end << ") }";
}

void Tokens::IdentifierTk::print(std::ostream& os) const {
    os << "IdentifierTk{ "
        << identifier
        << " (_" << span.line << " " << span.start << ":" << span.end << ") }";
}

void Tokens::IntTk::print(std::ostream& os) const {
    os << "IntTk{ "
        << value
        << " (_" << span.line << " " << span.start << ":" << span.end << ") }";
}

void Tokens::RealTk::print(std::ostream& os) const {
    os << "RealTk{ "
        << value
        << " (_" << span.line << " " << span.start << ":" << span.end << ") }";
}