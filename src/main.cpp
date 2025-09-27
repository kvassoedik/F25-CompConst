#include "lexer/Lexer.h"
#include "lexer/TokenList.h"
#include "ast/Parser.h"
#include <iostream>
#include <algorithm>

int main(int argc, char **argv) {
    if (argc < 2) {
        std::cerr << "Expected a file. See: silcc -h\n";
        return 2;
    }

    // Proccess flags
    int lx_verbosity = 0;
    for (int i = 1; i < argc-1; ++i) {
        std::string_view arg(argv[i]);

        if ("-h" == arg) {
            std::cout << "SILC language compiler. Usage:\n"
            "   silcc [options] <file>\n"
            "\n"
            "   options:\n"
            "       -h  - display help\n"
            "       -lx - configure Lexer\n"
            "           V    - configure log verbosity level [0-2]\n";

            return 0;
        } else if (arg.size() > 4 &&
            "-lx" == arg.substr(0, 3))
        {
            // Messages
            std::string_view option = arg.substr(3);
            if ('V' == option.at(0)) {
                lx_verbosity = std::clamp(std::atoi(option.substr(1).data()), 0, 2);
            } else {
                std::cerr << "Unrecognized lx option: " << option << "\n";
                return 1;
            }
        }
    }
    char* fileName = argv[argc - 1];

    // Lexer stage
    Lexer lexer(lx_verbosity);
    if (!lexer.openFile(fileName)) {
        std::cerr << "Could not open a file " << argv[1] << "\n";
        return 3;
    }

    std::vector<std::shared_ptr<Tokens::BaseTk>> tokens;
    try {
        tokens = std::move(lexer.scan());
    } catch(std::runtime_error e) {
        std::cerr << "------ UNEXPECTED EXCEPTION scanning file (lx): " << fileName << "\n" << e.what() << "\n";
        return 3;
    }
    if (lexer.releaseErrors()) {
        return 4;
    }

    std::cout << "======= TOKEN SEQUENCE =======\n";
    for (auto& t: tokens) {
        std::cout << *t << "\n";
    }
}