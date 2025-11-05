#pragma once

#include "FileReader.h"
#include "parser/fwd_structs.h"
#include "report/Report.h"
#include "analyzer/Optimizer.h"
#include <memory>
#include <unordered_map>

#define AST_VALIDATE_METHOD_SIGNATURE \
void validate(::analyzer::Analyzer& analyzer)
#define AST_VALIDATE_METHOD \
void validate(::analyzer::Analyzer& analyzer) final override { analyzer.validate(*this); }

class Parser;

namespace analyzer {

class Analyzer final {
public:
    Analyzer(std::shared_ptr<FileReader> file, Parser& parser, Optimizer& optimizer)
        : file_(std::move(file)), parser_(parser), optimizer_(optimizer) {}

    int configure(int* argc, char** argv);
    void run();
    bool hasErrors() const { return reporter_.hasErrors(); };

    void validate(Ast::TypeRef& node);
    void validate(Ast::TypeDecl& node);
    void validate(Ast::Block& node);
    void validate(Ast::IntRange& node);
    void validate(Ast::ArrayIdRange& node);
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
    std::shared_ptr<Ast::Decl> searchDeclaration(const std::string& id);
    void invalidateKnownVarsInCurrBlock();
    void invalidateKnownVarByRef(Ast::IdRef& node);
    void saveError(std::string reason, Tokens::Span span);
private:
    std::shared_ptr<FileReader> file_;
    std::shared_ptr<Ast::Block> root_{nullptr};
    Ast::Block* currBlock_;
    struct {
        Ast::IdRef* head{nullptr};
        Ast::ModifiablePrimary* prev;
        const std::shared_ptr<Ast::Type>* currType;
    } idRef_;
    Ast::Routine* currRoutine_{nullptr};
    std::unordered_map<std::string, std::shared_ptr<Ast::Routine>> undefinedRoutines_;
    Reporter reporter_{file_};
    Parser& parser_;
    Optimizer& optimizer_;
    bool deadCode_{false};
};

}