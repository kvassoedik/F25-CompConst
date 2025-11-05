#pragma once

#include "FileReader.h"

class Parser;

namespace analyzer {

class Optimizer final {
public:
    Optimizer(std::shared_ptr<FileReader> file, Parser& parser)
        : file_(file), parser_(parser) {}

    int configure(int* argc, char** argv);
    std::shared_ptr<Ast::Expr> computeExpr(Ast::Expr& expr);
    void removeUnusedDecls(Ast::Block& currBlock);

    enum class AssignmentOptStatus { Skip, Fail, Success };
    AssignmentOptStatus optimizeAssignmentAway(Ast::Assignment& node);
    void onBlockFinish(Ast::Block& currBlock);
private:
    struct Log {
        const std::string& msg;
        Tokens::Span span;
    };

    void log(const Log& log);
private:
    std::vector<const Ast::Entity*> unitsToBeRemoved_;
    std::shared_ptr<FileReader> file_;
    Parser& parser_;

    struct {
        struct {
            bool computations{false};
            bool unusedDecls{false};
        } logs;
        bool disabled{false};
    } config_;
};

}