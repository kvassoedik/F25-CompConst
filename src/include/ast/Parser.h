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
        : file_(std::move(file)),
        root_(Ast::mk<Ast::Block>(Tokens::Span{.line = 1, .start = 0, .end = file_->size()})),
        currBlock_(root_) {}

    int configure(int* argc, char** argv);
    void feed(TokenList tokens);
    void parse();
    bool hasErrors() const;
private:
    bool nextNode();
    void saveError(std::string reason, Tokens::Span span);

    std::pair<bool, std::shared_ptr<Tokens::BaseTk>> parseEntity();
    void finalizeCurrBlock();

    std::shared_ptr<Ast::Block> parseRoutineBody(Tokens::Span initSpan);
    void parseIfBody(std::shared_ptr<Ast::IfStmt>& parent, Tokens::Span initSpan);
    // No validation of 1st tk
    std::shared_ptr<Ast::Routine> parseRoutine();
    std::shared_ptr<Ast::Var> parseRoutineParam();
    std::shared_ptr<Ast::Var> parseVarDecl();
    // No validation of 1st tk
    std::shared_ptr<Ast::TypeDecl> parseTypeDecl();

    // Guarantees non-nullptr
    std::shared_ptr<Ast::Type> parseType();

    std::shared_ptr<Ast::Expr> parseExpr();
    std::shared_ptr<Ast::Expr> parseRelation();
    std::shared_ptr<Ast::Expr> parseSimple();
    std::shared_ptr<Ast::Expr> parseFactor();
    std::shared_ptr<Ast::Expr> parseSummand();
    std::shared_ptr<Ast::Expr> parsePrimary();
    std::shared_ptr<Ast::ModifiablePrimary> parseModifiablePrimary();
    // If successful, invalidates the param!
    std::shared_ptr<Ast::RoutineCall> parseRoutineCall(std::shared_ptr<Ast::ModifiablePrimary>& modif);

    // No validation of 1st tk
    std::shared_ptr<Ast::PrintStmt> parsePrintStmt();
    // No validation of 1st tk
    std::shared_ptr<Ast::IfStmt> parseIfStmt();
    // No validation of 1st tk
    std::shared_ptr<Ast::ForStmt> parseForStmt();
    // No validation of 1st tk
    std::shared_ptr<Ast::WhileStmt> parseWhileStmt();
    // No validation of 1st tk
    std::shared_ptr<Ast::ReturnStmt> parseReturnStmt();
    
    std::shared_ptr<Ast::RangeSpecifier> parseRangeSpecifier();
private:
    TokenList tokens_;
    std::vector<std::string> savedErrors_;
    std::shared_ptr<FileReader> file_;
    std::shared_ptr<Ast::Block> root_;
    std::shared_ptr<Ast::Block> currBlock_;
    std::shared_ptr<Tokens::BaseTk> startTk_;
    Reporter reporter_{file_};
};