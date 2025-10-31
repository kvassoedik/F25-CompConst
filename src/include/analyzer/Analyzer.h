#pragma once

#include "FileReader.h"
#include "parser/fwd_structs.h"
#include "report/Report.h"
#include <memory>

#define AST_VALIDATE_METHOD_SIGNATURE \
void validate(::Analyzer& analyzer)
#define AST_VALIDATE_METHOD \
void validate(::Analyzer& analyzer) final override { analyzer.validate(*this); }

class Analyzer final {
public:
    Analyzer(std::shared_ptr<FileReader> file)
        : file_(std::move(file)) {}

    int configure(int* argc, char** argv);
    void feed(std::shared_ptr<Ast::Block> ast) { root_ = std::move(ast); };
    void run();
    bool hasErrors() const { return reporter_.hasErrors(); };

    void validate(Ast::TypeRef& node);
    void validate(Ast::TypeDecl& node);
    void validate(Ast::Block& node);
    void validate(Ast::IntRange& node);
    void validate(Ast::ArrayId& node);
    void validate(Ast::IdRef& node);
    void validate(Ast::BinaryExpr& node);
    void validate(Ast::UnaryExpr& node);
    void validate(Ast::PrintStmt& node);
    void validate(Ast::IfStmt& node);
    void validate(Ast::WhileStmt& node);
    void validate(Ast::ForStmt& node);
    void validate(Ast::ReturnStmt& node);
    void validate(Ast::Assignment& node);
    void validate(Ast::Var& node);
    void validate(Ast::Routine& node);
    void validate(Ast::RoutineCall& node);
    void validate(Ast::ArrayType& node);
    void validate(Ast::ArrayAccess& node);
    void validate(Ast::RecordType& node);
private:
    bool areTypesEqual(const std::shared_ptr<Ast::Type>& t1, const std::shared_ptr<Ast::Type>& t2);
    std::string stringifyType(const std::shared_ptr<Ast::Type>& t);
    Ast::Decl* searchDeclaration(const std::string& id);

    void saveError(std::string reason, Tokens::Span span);
private:
    std::shared_ptr<FileReader> file_;
    std::shared_ptr<Ast::Block> root_{nullptr};
    Ast::Block* currBlock_;
    Reporter reporter_{file_};
};