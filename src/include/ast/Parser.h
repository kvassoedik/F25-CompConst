#pragma once

#include "Ast.h"
#include "lexer/Token.h"
#include "lexer/TokenList.h"
#include <memory>

class Parser final {
public:
    int configure(int* argc, char** argv);
    void feed(TokenList tokens);
    void parse();
private:
    bool nextNode();
    void saveError(std::string reason);
    std::shared_ptr<Ast::Decl> findDeclaration(const std::string& id);
    std::shared_ptr<Ast::Type> findNamedType(const std::string& id);

    void parseBlock(std::shared_ptr<Ast::Block>& parent);
    void parseRoutine();
    void parseRoutineParam(std::shared_ptr<Ast::RoutineDecl>& parent);

    std::shared_ptr<Ast::Type> parseType();
private:
    TokenList tokens_;
    std::shared_ptr<Ast::Block> root_ = std::make_shared<Ast::Block>();
    std::shared_ptr<Ast::Block> currBlock_{root_};
};