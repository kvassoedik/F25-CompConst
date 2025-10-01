#pragma once

#include "Ast.h"
#include "lexer/Token.h"
#include "lexer/TokenList.h"
#include "FileReader.h"
#include "report/Report.h"
#include <memory>

class Parser final {
public:
    Parser(std::shared_ptr<FileReader> file)
        : file_(std::move(file)) {}

    int configure(int* argc, char** argv);
    void feed(TokenList tokens);
    void parse();
    bool releaseErrors();
private:
    bool nextNode();
    void saveError(Tokens::Span span, std::string reason);
    std::shared_ptr<Ast::Decl> findDeclaration(const std::string& id);
    std::shared_ptr<Ast::Type> findNamedType(const std::string& id);

    void parseBlock(std::shared_ptr<Ast::Block>& parent);
    void parseRoutine();
    void parseRoutineParam(std::shared_ptr<Ast::RoutineDecl>& parent);

    std::shared_ptr<Ast::Type> parseType();
private:
    TokenList tokens_;
    std::vector<std::string> savedErrors_;
    std::shared_ptr<Ast::Block> root_ = std::make_shared<Ast::Block>();
    std::shared_ptr<Ast::Block> currBlock_{root_};
    std::shared_ptr<FileReader> file_;
    std::shared_ptr<Tokens::BaseTk> startTk_;
    Reporter reporter_{file_};
};