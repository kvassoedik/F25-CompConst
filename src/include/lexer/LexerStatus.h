#pragma once

enum class LexerStatus {
    NONE = 0,
    LX_UNSUPPORTED_SYMBOL,
    LX_NUMBER_WITH_LETTER,
    LX_REAL_MANY_DOTS,
    LX_INT_EXCEED,
    LX_REAL_EXCEED,
    LX_CR_NO_LF,
    LX_NON_ASCII,
};