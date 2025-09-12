#pragma once

enum class LexerStatus {
    NONE = 0,
    UNRECOGNIZED_SYMBOL,
    NUMBER_WITH_LETTER,
    REAL_MANY_DOTS,
    INT_EXCEED,
    REAL_EXCEED,
    CR_NO_LF,
};