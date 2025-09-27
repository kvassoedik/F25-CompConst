#include "lexer/Lexer.h"
#include "lexer/TokenList.h"
#include "ast/Parser.h"
#include <iostream>

int main(int argc, char **argv) {
    if (argc < 2) {
        std::cerr << "Expected a file. See: silcc -h\n";
        return 1;
    }

    // Proccess flags
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
        }
    }
    char* fileName = argv[argc - 1];

    Lexer lexer;
    if (!lexer.openFile(fileName)) {
        std::cerr << "Could not open a file " << argv[1] << "\n";
        return 2;
    }
    if (lexer.configure(&argc, argv) != 0)
        return 3;

    Parser parser;
    if (parser.configure(&argc, argv) != 0)
        return 3;

    // Lexer stage
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

    // std::cout << "======= TOKEN SEQUENCE =======\n";
    // for (auto& t: tokens) {
    //     std::cout << *t << "\n";
    // }

    // Excluding EOF token
    if (!tokens.empty() && tokens[tokens.size() - 1]->type == TokenType::END_OF_FILE)
        tokens.pop_back();

    parser.feed(TokenList(std::move(tokens)));
    parser.parse();
}