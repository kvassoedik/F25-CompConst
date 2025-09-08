#include "lexer/Token.h"
#include <sstream>

std::ostream& operator<<(std::ostream &os, const Tokens::BaseTk &o) {
    switch (static_cast<int>(o.type))
    {
    case 1: return ::operator <<(os, static_cast<const Tokens::IdentifierTk&>(o));
    case 2: return ::operator <<(os, static_cast<const Tokens::IntTk&>(o));
    case 3: return ::operator <<(os, static_cast<const Tokens::RealTk&>(o));
    default: os << "BaseTk{ " << static_cast<unsigned int>(o.type)
        << " (l" << o.span.line << " " << o.span.start << ":" << o.span.end << ") }";
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, const Tokens::IdentifierTk& o) {
    os << "IdentifierTk{ " //<< o.identifier
        << " (l" << o.span.line << " " << o.span.start << ":" << o.span.end << ") }";
    return os;
}

std::ostream& operator<<(std::ostream& os, const Tokens::IntTk& o) {
    os << "IntTk{ " << o.value
        << " (l" << o.span.line << " " << o.span.start << ":" << o.span.end << " ) }";
    return os;
}

std::ostream& operator<<(std::ostream& os, const Tokens::RealTk& o) {
    os << "RealTk{ " << o.value
        << " (l" << o.span.line << " " << o.span.start << ":" << o.span.end << " ) }";
    return os;
}