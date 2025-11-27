#pragma once

// Usage: ANSI_START ... ANSI_APPLY <your_text> ANSI_RESET
#define ANSI_START "\033["
#define ANSI_APPLY "m"
#define ANSI_AND ";"

#define ANSI_RED "31"
#define ANSI_GREEN "32"
#define ANSI_YELLOW "33"
#define ANSI_BLUE "34"
#define ANSI_BOLD "1"

#define ANSI_RESET "\033[0m"