#pragma once

#include "parser/debug.h"
#include "parser/fwd_structs.h"
#include "utils/PrintingUtils.h"
#include <iostream>
#include <memory>
#include <unordered_set>
#include <vector>
#include <queue>

#define AST_DEBUGTREE_PRINT_METHOD_SIGNATURE
#define AST_DEBUGTREE_PRINT_METHOD

#if AST_DEBUG_ON

#undef AST_DEBUGTREE_PRINT_METHOD_SIGNATURE
#define AST_DEBUGTREE_PRINT_METHOD_SIGNATURE \
void print(::Ast::DebugTree& debugTree)

#undef AST_DEBUGTREE_PRINT_METHOD
#define AST_DEBUGTREE_PRINT_METHOD \
void print(::Ast::DebugTree& debugTree) override { debugTree.print(*this); }

namespace Ast {

class DebugTree final {
public:
    DebugTree();
    void newNode(std::shared_ptr<Ast::Entity> node);
    void printAll();
    void pushPrint(unsigned long debugId);

    // ---------- Node-specific printing methods ----------
    
    void print(Ast::Entity& node);
    void print(Ast::Type& node);
    void print(Ast::Expr& node);
    void print(Ast::RangeSpecifier& node);
    void print(Ast::Block& node);
    void print(Ast::Decl& node);
    void print(Ast::TypeRef& node);
    void print(Ast::TypeDecl& node);
    void print(Ast::ArrayType& node);
    void print(Ast::RecordType& node);
    void print(Ast::IntRange& node);
    void print(Ast::ArrayIdRange& node);
    void print(Ast::ArrayAccess& node);
    void print(Ast::ModifiablePrimary& node);
    void print(Ast::IdRef& node);
    void print(Ast::BoolLiteral& node);
    void print(Ast::IntLiteral& node);
    void print(Ast::RealLiteral& node);
    void print(Ast::BinaryExpr& node);
    void print(Ast::UnaryExpr& node);
    void print(Ast::PrintStmt& node);
    void print(Ast::IfStmt& node);
    void print(Ast::WhileStmt& node);
    void print(Ast::ForStmt& node);
    void print(Ast::ReturnStmt& node);
    void print(Ast::Assignment& node);
    void print(Ast::Var& node);
    void print(Ast::Routine& node);
    void print(Ast::RoutineCall& node);
    void print(Ast::RoutineType& node);

private:
    void printImpl(const std::shared_ptr<Ast::Entity>& node, const std::string& prefix);
private:
    std::vector<std::shared_ptr<Ast::Entity>> nodes_;

    struct DepthElem {
        unsigned long debugId;
        unsigned int depth;
    };
    class DepthComparator {
    public:
        bool operator() (DepthElem a, DepthElem b) {
            return a.depth < b.depth || (a.depth == b.depth && a.debugId > b.debugId);
        }
    };

    std::priority_queue<DepthElem, std::vector<DepthElem>, DepthComparator> depthStack_;
    std::unordered_set<unsigned long> alreadyDisplayed_;
    std::string newline_;
    std::ostream& os_;
    unsigned long globalDebugId_{1};
    unsigned int depth_{0};
    bool depthIncrement_{false};
    bool isCurrOrphan_{false};
    const Ast::Entity* nextModifPrimary_{nullptr};
};

// GLOBAL VARIABLE
inline DebugTree debugInfo;

}
#endif
