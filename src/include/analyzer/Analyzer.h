#pragma once

#include "analyzer/Optimizer.h"
#include "report/Report.h"
#include <memory>
#include <unordered_map>

#define AST_VALIDATE_METHOD_SIGNATURE \
void validate(::analyzer::Analyzer& analyzer)
#define AST_VALIDATE_METHOD \
void validate(::analyzer::Analyzer& analyzer) final override { analyzer.validate(*this); }

namespace analyzer {

class Analyzer final {
public:
    Analyzer(std::shared_ptr<FileReader> file, std::shared_ptr<ast::Ast> ast, Optimizer& optimizer)
        : file_(std::move(file)), ast_(std::move(ast)), optimizer_(optimizer) {}

    int configure(int* argc, char** argv);
    void run();
    bool hasErrors() const { return reporter_.hasErrors(); };

    void validate(ast::TypeRef& node);
    void validate(ast::TypeDecl& node);
    void validate(ast::Block& node);
    void validate(ast::IntRange& node);
    void validate(ast::ArrayIdRange& node);
    void validate(ast::IdRef& node);
    void validate(ast::RecordMember& node);
    void validate(ast::BinaryExpr& node);
    void validate(ast::UnaryExpr& node);
    void validate(ast::PrintStmt& node);
    void validate(ast::IfStmt& node);
    void validate(ast::WhileStmt& node);
    void validate(ast::ForStmt& node);
    void validate(ast::ReturnStmt& node);
    void validate(ast::Assignment& node);
    void validate(ast::Var& node);
    void validate(ast::Routine& node);
    void validate(ast::RoutineCall& node);
    void validate(ast::ArrayType& node);
    void validate(ast::ArrayAccess& node);
    void validate(ast::RecordType& node);
private:
    std::shared_ptr<ast::Decl> searchDeclaration(const std::string& id);
    void invalidateKnownVarsInCurrBlock();
    void invalidateKnownVarByRef(ast::IdRef& node);
    void saveError(std::string reason, Tokens::Span span);
    void validateType(std::shared_ptr<ast::Type>& t);
    bool isInGlobalScope() const noexcept { return currBlock_ == ast_->getRoot().get(); }
private:
    std::shared_ptr<FileReader> file_;
    std::shared_ptr<ast::Ast> ast_;
    Optimizer& optimizer_;

    ast::Block* currBlock_;
    struct {
        ast::Primary* head{nullptr};
        ast::Primary* prev;
        const std::shared_ptr<ast::Type>* currType;
    } idRef_;
    ast::Routine* currRoutine_{nullptr};
    std::unordered_map<std::string, std::shared_ptr<ast::Routine>> undefinedRoutines_;
    std::unordered_set<std::string> recordMemberNames_;
    Reporter reporter_{file_};
    bool deadCode_{false};
    bool analyzingRoutineParams_{false};
};

}