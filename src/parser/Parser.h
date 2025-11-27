#pragma once

#include "parser/Ast.h"
#include "lexer/Tokens.h"
#include "lexer/TokenList.h"
#include "utils/FileReader.h"
#include "utils/report/Report.h"
#include <memory>

class Parser final {
public:
    Parser(std::shared_ptr<FileReader> file, std::shared_ptr<ast::Ast> ast)
        : file_(std::move(file)), ast_(std::move(ast)), currBlock_(ast_->getRoot()) {}

    int configure(int* argc, char** argv);
    void feed(TokenList tokens) { tokens_ = std::move(tokens); }
    void run();
    bool hasErrors() const { return reporter_.hasErrors(); }
private:
    bool nextNode();
    void saveError(std::string reason, Tokens::Span span);

    std::pair<bool, std::shared_ptr<Tokens::BaseTk>> parseEntity();
    void finalizeCurrBlock();

    std::shared_ptr<ast::Block> parseRoutineBody(Tokens::Span initSpan);
    void parseIfBody(std::shared_ptr<ast::IfStmt>& parent, Tokens::Span initSpan);
    // No validation of 1st tk
    std::shared_ptr<ast::Routine> parseRoutine();
    std::shared_ptr<ast::Var> parseRoutineParam();
    std::shared_ptr<ast::Var> parseVarDecl();
    // No validation of 1st tk
    std::shared_ptr<ast::TypeDecl> parseTypeDecl();

    // Guarantees non-nullptr
    std::shared_ptr<ast::Type> parseType();

    std::shared_ptr<ast::Expr> parseExpr();
    std::shared_ptr<ast::Expr> parseRelation();
    std::shared_ptr<ast::Expr> parseSimple();
    std::shared_ptr<ast::Expr> parseFactor();
    std::shared_ptr<ast::Expr> parseSummand();
    std::shared_ptr<ast::Expr> parseSecondary();
    std::shared_ptr<ast::Primary> parsePrimary();
    // If successful, invalidates the param!
    std::shared_ptr<ast::RoutineCall> parseRoutineCall(std::shared_ptr<Tokens::IdentifierTk>& id);

    // No validation of 1st tk
    std::shared_ptr<ast::PrintStmt> parsePrintStmt();
    // No validation of 1st tk
    std::shared_ptr<ast::IfStmt> parseIfStmt();
    // No validation of 1st tk
    std::shared_ptr<ast::ForStmt> parseForStmt();
    // No validation of 1st tk
    std::shared_ptr<ast::WhileStmt> parseWhileStmt();
    // No validation of 1st tk
    std::shared_ptr<ast::ReturnStmt> parseReturnStmt();
    
    std::shared_ptr<ast::RangeSpecifier> parseRangeSpecifier();
private:
    TokenList tokens_;
    std::vector<std::string> savedErrors_;
    std::shared_ptr<FileReader> file_;
    std::shared_ptr<ast::Ast> ast_;
    std::shared_ptr<ast::Block> currBlock_;
    std::shared_ptr<Tokens::BaseTk> startTk_;
    Reporter reporter_{file_};
};