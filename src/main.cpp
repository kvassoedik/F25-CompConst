#include "lexer/Lexer.h"
#include <iostream>

int main(int argc, char **argv) {
    if (argc < 2) {
        std::cerr << "Expected a file. Usage: silcc [flags] <file>\n";
        return 1;
    }

    // Proccess flags
    char* fileName = argv[argc - 1];

    // Lexer stage
    Lexer lexer;
    if (!lexer.openFile(fileName)) {
        std::cerr << "Could not open a file " << argv[1] << "\n";
        return 2;
    }

    std::vector<Tokens::BaseTk> tokens;
    try {
        tokens = lexer.scan();
    } catch(std::runtime_error e) {
        std::cerr << "Lexer failed: " << e.what() << "\n";
        return 3;
    }

    for (auto& t: tokens) {
        t.print(std::cout);
        std::cout << "\n";
    }
}