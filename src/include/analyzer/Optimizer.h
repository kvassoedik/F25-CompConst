#pragma once

#include "parser/Ast.h"
#include "FileReader.h"

namespace analyzer {

class Optimizer final {
public:
    Optimizer(std::shared_ptr<FileReader> file, std::shared_ptr<ast::Ast> ast)
        : file_(file), ast_(std::move(ast)) {}

    int configure(int* argc, char** argv);
    std::shared_ptr<ast::Expr> computeExpr(ast::Expr& expr);
    void onBlockFinish(ast::Block& currBlock);
    void removeUnitFromCurrBlockLater(const ast::Entity& node);
    void removeUnusedDecls(ast::Block& currBlock);

    enum class AssignmentOptStatus { Skip, Fail, Success };
    AssignmentOptStatus optimizeAssignmentAway(ast::Assignment& node, std::shared_ptr<ast::Decl>& decl, bool firstTimeUsed);
private:
    struct Log {
        const std::string& msg;
        Tokens::Span span;
    };

    void log(const Log& log);
private:
    std::vector<const ast::Entity*> unitsToBeRemoved_;
    std::shared_ptr<FileReader> file_;
    std::shared_ptr<ast::Ast> ast_;

    struct {
        struct {
            bool computations{false};
            bool unusedDecls{false};
        } logs;
        struct {
            bool computations{true};
            bool unusedDecls{true};
        } toggles;
    } config_;
};

}