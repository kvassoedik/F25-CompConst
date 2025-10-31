#pragma once

#include "parser/Entity.h"
#include "parser/DebugTree.h"
#include <vector>
#include <list>
#include <memory>
#include <unordered_map>
#include <algorithm>

namespace Ast {

struct Decl : public Entity {
    Decl(Tokens::Span span, std::string id)
        : Entity(span), id(std::move(id)) {}

#if AST_DEBUG_ON
    AST_DEBUGTREE_PRINT_METHOD_SIGNATURE override = 0;
#endif
public:
    std::string id;
    bool isRoutine: 1 = false; // declarations can be either routine or var
};

enum class TypeEnum {
    ERROR,
    RESOLVABLE,

    Int,
    Real,
    Bool,

    Array,
    Record,
    Routine,
};

struct Type : public Entity {
    Type(Tokens::Span span, TypeEnum code)
        : Entity(span), code(code) {}

    AST_DEBUGTREE_PRINT_METHOD
public:
    TypeEnum code;
};

struct TypeRef final : public Type {
    TypeRef(Tokens::Span span, std::string id)
        : Type(span, TypeEnum::RESOLVABLE), id(std::move(id)) {}
    
    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::string id;
};

struct TypeDecl final : public Decl {
    TypeDecl(Tokens::Span span, std::string id)
        : Decl(span, std::move(id)) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::shared_ptr<Type> type{nullptr};
};

struct Block final : public Entity {
    Block(Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::list<std::shared_ptr<Entity>> units;
    std::unordered_map<std::string, Decl&> declMap;
    std::unordered_map<std::string, Type&> typeMap;
    std::shared_ptr<Block> parent{nullptr};
};

/************************************ Expr ************************************/

enum class ExprEnum {
    ERROR,

    BoolLiteral,
    IntLiteral,
    RealLiteral,

    IdRef,
    ArrayAccess,

    Negate,
    Not,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    And,
    Or,
    Xor,
    RoutineCall,

    LESS_THAN,
    LESS_OR_EQUAL,
    MORE_THAN,
    MORE_OR_EQUAL,
    EQUAL,
    UNEQUAL,
};

struct Expr : public Entity {
    Expr(Tokens::Span span)
        : Entity(span) {}
    Expr(Tokens::Span span, ExprEnum code, std::shared_ptr<Type> type = nullptr)
        : Entity(span), code(code), type(std::move(type)) {}

    AST_DEBUGTREE_PRINT_METHOD
public:
    std::shared_ptr<Type> type{nullptr};
    std::weak_ptr<Entity> parent;
    ExprEnum code;
    bool known: 1 = false;
};

struct RangeSpecifier : public Entity {
    RangeSpecifier(Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
};
struct IntRange final : public RangeSpecifier {
    IntRange(Tokens::Span span)
        : RangeSpecifier(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::shared_ptr<Expr> start{nullptr};
    std::shared_ptr<Expr> end{nullptr};
};
struct ArrayId final : public RangeSpecifier {
    ArrayId(Tokens::Span span)
        : RangeSpecifier(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::string id;
};

struct ModifiablePrimary : public Expr {
    ModifiablePrimary(Tokens::Span span)
        : Expr(span) {}

    AST_DEBUGTREE_PRINT_METHOD
public:
    std::shared_ptr<ModifiablePrimary> next{nullptr};
};

struct IdRef final: public ModifiablePrimary {
    IdRef(Tokens::Span span, std::string id)
        : ModifiablePrimary(span), id(std::move(id)) {
            code = ExprEnum::IdRef;
        }

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::string id;
    std::shared_ptr<Decl> ref{nullptr};
};

struct BoolLiteral final: public Expr {
    BoolLiteral(Tokens::Span span, bool val)
        : Expr(span, ExprEnum::BoolLiteral), val(val) {}

    AST_DEBUGTREE_PRINT_METHOD
public:
    bool val;
};
struct IntLiteral final: public Expr {
    IntLiteral(Tokens::Span span, long val)
        : Expr(span, ExprEnum::IntLiteral), val(val) {}

    AST_DEBUGTREE_PRINT_METHOD
public:
    long val;
};
struct RealLiteral final: public Expr {
    RealLiteral(Tokens::Span span, double val)
        : Expr(span, ExprEnum::RealLiteral), val(val) {}

    AST_DEBUGTREE_PRINT_METHOD
public:
    double val;
};

struct BinaryExpr final: public Expr {
    BinaryExpr(Tokens::Span span, ExprEnum code)
        : Expr(span, code) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::shared_ptr<Expr> left{nullptr};
    std::shared_ptr<Expr> right{nullptr};
};
struct UnaryExpr final: public Expr {
    UnaryExpr(Tokens::Span span, ExprEnum code)
        : Expr(span, code) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::shared_ptr<Expr> val{nullptr};
};

/************************************ Statement ************************************/

struct PrintStmt final : public Entity {
    PrintStmt(Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::vector<std::shared_ptr<Expr>> args;
};

struct IfStmt final : public Entity {
    IfStmt(Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::shared_ptr<Expr> condition{nullptr};
    std::shared_ptr<Block> body{nullptr};
    std::shared_ptr<Block> elseBody{nullptr};
};

struct WhileStmt final : public Entity {
    WhileStmt(Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::shared_ptr<Expr> condition{nullptr};
    std::shared_ptr<Block> body{nullptr};
};

struct ForStmt final : public Entity {
    ForStmt(Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::string counterId;
    std::shared_ptr<RangeSpecifier> range{nullptr};
    std::shared_ptr<Block> body{nullptr};
    bool reverse;
};

struct ReturnStmt final : public Entity {
    ReturnStmt(Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::shared_ptr<Expr> val{nullptr};
};

struct Assignment final : public Entity {
    Assignment(Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::shared_ptr<ModifiablePrimary> left{nullptr};
    std::shared_ptr<Expr> val{nullptr};
};

/************************************ Var ************************************/

struct Var final : public Decl {
    Var(Tokens::Span span, std::string id)
        : Decl(span, std::move(id)) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::shared_ptr<Type> type{nullptr};
    std::shared_ptr<Expr> val{nullptr};
    bool initialized: 1 = false;
};

/************************************ Routine ************************************/

struct Routine final : public Decl {
    Routine(Tokens::Span span, std::string id)
        : Decl(span, std::move(id)) {
            isRoutine = true;
        }

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::vector<std::shared_ptr<Var>> params;
    std::shared_ptr<Type> retType{nullptr};
    std::shared_ptr<Block> body{nullptr};
};

struct RoutineCall final : public Expr {
    RoutineCall(Tokens::Span span, std::string routineId)
        : Expr(span, ExprEnum::RoutineCall), routineId(std::move(routineId)) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::vector<std::shared_ptr<Expr>> args;
    std::string routineId;
};

/************************************ Array ************************************/

struct ArrayType final: public Type {
    ArrayType(Tokens::Span span)
        : Type(span, TypeEnum::Array) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::shared_ptr<Expr> size{nullptr};
    std::shared_ptr<Type> elemType{nullptr};
};

struct ArrayAccess final : public ModifiablePrimary {
    ArrayAccess(Tokens::Span span)
        : ModifiablePrimary(span) {
            code = ExprEnum::ArrayAccess;
        }

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::shared_ptr<Expr> val{nullptr};
};

/************************************ Record ************************************/

struct RecordType final : public Type {
    RecordType(Tokens::Span span)
        : Type(span, TypeEnum::Record) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::vector<std::shared_ptr<Var>> members;
};

}
