#pragma once

#include "parser/debug.h"

#define AST_DEBUGTREE_PRINT_METHOD_SIGNATURE
#define AST_DEBUGTREE_PRINT_METHOD

#if AST_DEBUG_ON

#include "parser/fwd_structs.h"
#include "utils/PrintingUtils.h"
#include <iostream>
#include <memory>
#include <unordered_set>
#include <vector>
#include <queue>
#include <list>

#undef AST_DEBUGTREE_PRINT_METHOD_SIGNATURE
#define AST_DEBUGTREE_PRINT_METHOD_SIGNATURE \
void print(::ast::DebugTree& debugTree)

#undef AST_DEBUGTREE_PRINT_METHOD
#define AST_DEBUGTREE_PRINT_METHOD \
void print(::ast::DebugTree& debugTree) override { debugTree.print(*this); }

namespace ast {

class DebugTree final {
public:
    DebugTree();
    void newNode(std::shared_ptr<Entity> node);
    void printAll();
    void pushPrint(unsigned long debugId);

    // ---------- Node-specific printing methods ----------
    
    void print(Entity& node);
    void print(Type& node);
    void print(Expr& node);
    void print(RangeSpecifier& node);
    void print(Block& node);
    void print(Decl& node);
    void print(TypeRef& node);
    void print(TypeDecl& node);
    void print(ArrayType& node);
    void print(RecordType& node);
    void print(RecordMember& node);
    void print(IntRange& node);
    void print(ArrayIdRange& node);
    void print(ArrayAccess& node);
    void print(Primary& node);
    void print(IdRef& node);
    void print(BoolLiteral& node);
    void print(IntLiteral& node);
    void print(RealLiteral& node);
    void print(BinaryExpr& node);
    void print(UnaryExpr& node);
    void print(PrintStmt& node);
    void print(IfStmt& node);
    void print(WhileStmt& node);
    void print(ForStmt& node);
    void print(ReturnStmt& node);
    void print(Assignment& node);
    void print(Var& node);
    void print(Routine& node);
    void print(RoutineCall& node);
    void print(RoutineType& node);

private:
    void printImpl(const std::shared_ptr<Entity>& node, const std::string& prefix);
private:
    std::vector<std::shared_ptr<Entity>> nodes_;

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
    std::list<const Entity*> nextPrimary_;
};

// GLOBAL VARIABLE
inline DebugTree debugInfo;

}
#endif
