#pragma once

#include "parser/Entity.h"
#include "parser/Ast.h"
#include "parser/srlz.h"
#include <vector>
#include <list>
#include <memory>
#include <unordered_map>
#include <algorithm>

namespace ast {

struct Decl : public Entity, public std::enable_shared_from_this<Decl> {
    Decl(const Ast& ast, Tokens::Span span, std::string id)
        : Entity(span), id(std::move(id)) {
            // by default, all types stored initially are all error
            // to avoid unexpected behaviour when scaling
            type = ast.getBaseTypes().error;
        }

#if AST_DEBUG_ON
    AST_DEBUGTREE_PRINT_METHOD_SIGNATURE override = 0;
#endif
public:
    std::string id;
    std::shared_ptr<Type> type;
    unsigned useCount{0};
    bool isRoutine{false}; // declarations can be either routine or var
};

enum class TypeEnum {
    ERROR,
    REFERENCE,

    Int,
    Real,
    Bool,

    Array,
    Record,
    Routine,
};

struct Type : public Entity {
    Type(const Ast& ast, Tokens::Span span, TypeEnum code)
        : Entity(span), code(code) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_SRLZTYPE_METHOD
    virtual CODEGEN_TYPE_METHOD
public:
    TypeEnum code;
};

struct TypeRef final : public Type {
    TypeRef(const Ast& ast, Tokens::Span span, std::string id)
        : Type(ast, span, TypeEnum::REFERENCE), id(std::move(id)) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_SRLZTYPE_METHOD
    AST_VALIDATE_METHOD
public:
    std::string id;
    std::weak_ptr<TypeDecl> ref;
};

struct TypeDecl final : public Decl {
    TypeDecl(const Ast& ast, Tokens::Span span, std::string id)
        : Decl(ast, span, std::move(id)) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
};

struct Block final : public Entity {
    Block(const Ast& ast, Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::list<std::shared_ptr<Entity>> units;
    std::unordered_map<std::string, std::shared_ptr<Decl>> declMap;
    std::unordered_map<std::string, std::shared_ptr<TypeDecl>> typeMap;
    std::weak_ptr<Block> parent;
};

/************************************ Expr ************************************/

enum class ExprEnum {
    ERROR,

    BoolLiteral,
    IntLiteral,
    RealLiteral,

    IdRef,
    RecordMember,
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
    Expr(const Ast& ast, Tokens::Span span, ExprEnum code)
        : Entity(span), code(code) {
            type = ast.getBaseTypes().error;
        }

    AST_DEBUGTREE_PRINT_METHOD
    CODEGEN_METHOD
public:
    std::shared_ptr<Type> type;
    ExprEnum code;
    bool knownPrimitive{false};
};

struct RangeSpecifier : public Entity {
    RangeSpecifier(const Ast& ast, Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
};
struct IntRange final : public RangeSpecifier {
    IntRange(const Ast& ast, Tokens::Span span)
        : RangeSpecifier(ast, span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::shared_ptr<Expr> start{nullptr};
    std::shared_ptr<Expr> end{nullptr};
};
struct ArrayIdRange final : public RangeSpecifier {
    ArrayIdRange(const Ast& ast, Tokens::Span span)
        : RangeSpecifier(ast, span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
public:
    std::string id;
    std::weak_ptr<Var> ref;
};

struct Primary : public Expr {
    Primary(const Ast& ast, Tokens::Span span)
        : Expr(ast, span, ExprEnum::ERROR) {}

    AST_DEBUGTREE_PRINT_METHOD
public:
    std::shared_ptr<Primary> next{nullptr};
};

struct IdRef final: public Primary {
    IdRef(const Ast& ast, Tokens::Span span, std::string id)
        : Primary(ast, span), id(std::move(id)) {
            code = ExprEnum::IdRef;
        }

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_METHOD
public:
    std::string id;
    std::weak_ptr<Decl> ref;
};

struct BoolLiteral final: public Expr {
    BoolLiteral(const Ast& ast, Tokens::Span span, bool val)
        : Expr(ast, span, ExprEnum::BoolLiteral), val(val) {
            knownPrimitive = true;
            type = ast.getBaseTypes().boolean;
        }

    AST_DEBUGTREE_PRINT_METHOD
public:
    bool val;
#if AST_DEBUG_ON
    bool debug_optimized{false};
#endif
};
struct IntLiteral final: public Expr {
    IntLiteral(const Ast& ast, Tokens::Span span, long val)
        : Expr(ast, span, ExprEnum::IntLiteral), val(val) {
            knownPrimitive = true;
            type = ast.getBaseTypes().integer;
        }

    AST_DEBUGTREE_PRINT_METHOD
public:
    long val;
#if AST_DEBUG_ON
    bool debug_optimized{false};
#endif
};
struct RealLiteral final: public Expr {
    RealLiteral(const Ast& ast, Tokens::Span span, double val)
        : Expr(ast, span, ExprEnum::RealLiteral), val(val) {
            knownPrimitive = true;
            type = ast.getBaseTypes().real;
        }

    AST_DEBUGTREE_PRINT_METHOD
public:
    double val;
#if AST_DEBUG_ON
    bool debug_optimized{false};
#endif
};

struct BinaryExpr final: public Expr {
    BinaryExpr(const Ast& ast, Tokens::Span span, ExprEnum code)
        : Expr(ast, span, code) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_METHOD
public:
    std::shared_ptr<Expr> left{nullptr};
    std::shared_ptr<Expr> right{nullptr};
};
struct UnaryExpr final: public Expr {
    UnaryExpr(const Ast& ast, Tokens::Span span, ExprEnum code)
        : Expr(ast, span, code) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_METHOD
public:
    std::shared_ptr<Expr> val{nullptr};
};

/************************************ Statement ************************************/

struct PrintStmt final : public Entity {
    PrintStmt(const Ast& ast, Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_METHOD
public:
    std::vector<std::shared_ptr<Expr>> args;
};

struct IfStmt final : public Entity {
    IfStmt(const Ast& ast, Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_METHOD
public:
    std::shared_ptr<Expr> condition{nullptr};
    std::shared_ptr<Block> body{nullptr};
    std::shared_ptr<Block> elseBody{nullptr};
};

struct WhileStmt final : public Entity {
    WhileStmt(const Ast& ast, Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_METHOD
public:
    std::shared_ptr<Expr> condition{nullptr};
    std::shared_ptr<Block> body{nullptr};
};

struct ForStmt final : public Entity {
    ForStmt(const Ast& ast, Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_METHOD
public:
    std::shared_ptr<Var> counter;
    std::shared_ptr<RangeSpecifier> range{nullptr};
    std::shared_ptr<Block> body{nullptr};
    bool reverse;
};

struct ReturnStmt final : public Entity {
    ReturnStmt(const Ast& ast, Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_METHOD
public:
    std::shared_ptr<Expr> val{nullptr};
};

struct Assignment final : public Entity {
    Assignment(const Ast& ast, Tokens::Span span)
        : Entity(span) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_METHOD
public:
    std::shared_ptr<Primary> left{nullptr};
    std::shared_ptr<Expr> val{nullptr};
};

/************************************ Var ************************************/

struct Var final : public Decl {
    Var(const Ast& ast, Tokens::Span span, std::string id)
        : Decl(ast, span, std::move(id)) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_METHOD
public:
    std::shared_ptr<Expr> val{nullptr};
    bool knownPrimitive{false};
};

/************************************ Routine ************************************/

struct Routine final : public Decl {
    Routine(const Ast& ast, Tokens::Span span, std::string id)
        : Decl(ast, span, std::move(id)) {
            isRoutine = true;
        }

    const std::shared_ptr<RoutineType>& getType() const noexcept { return (std::shared_ptr<RoutineType>&)(type); }
    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_METHOD
public:
    std::shared_ptr<Block> body{nullptr};
};

struct RoutineCall final : public Primary {
    RoutineCall(const Ast& ast, Tokens::Span span, std::string routineId)
        : Primary(ast, span), routineId(std::move(routineId)) {
            code = ExprEnum::RoutineCall;
        }

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_METHOD
public:
    std::vector<std::shared_ptr<Expr>> args;
    std::string routineId;
    std::weak_ptr<Routine> ref;
};

struct RoutineType final : public Type {
    RoutineType(const Ast& ast, Tokens::Span span)
        : Type(ast, span, TypeEnum::Routine) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_SRLZTYPE_METHOD
    CODEGEN_TYPE_METHOD
public:
    std::vector<std::shared_ptr<Var>> params;
    std::shared_ptr<Type> retType{nullptr};
};

/************************************ Array ************************************/

struct ArrayType final: public Type {
    ArrayType(const Ast& ast, Tokens::Span span)
        : Type(ast, span, TypeEnum::Array) {
            elemType = ast.getBaseTypes().error;
        }

    AST_DEBUGTREE_PRINT_METHOD
    AST_SRLZTYPE_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_TYPE_METHOD
public:
    std::shared_ptr<Expr> size{nullptr};
    std::shared_ptr<Type> elemType;
};

struct ArrayAccess final : public Primary {
    ArrayAccess(const Ast& ast, Tokens::Span span)
        : Primary(ast, span) {
            code = ExprEnum::ArrayAccess;
        }

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_METHOD
public:
    std::shared_ptr<Expr> val{nullptr};
};

/************************************ Record ************************************/

struct RecordType final : public Type {
    RecordType(const Ast& ast, Tokens::Span span)
        : Type(ast, span, TypeEnum::Record) {}

    AST_DEBUGTREE_PRINT_METHOD
    AST_SRLZTYPE_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_TYPE_METHOD
public:
    std::vector<std::shared_ptr<Var>> members;
};

struct RecordMember final: public Primary {
    RecordMember(const Ast& ast, Tokens::Span span, std::string id)
        : Primary(ast, span), id(std::move(id)) {
            code = ExprEnum::RecordMember;
        }

    AST_DEBUGTREE_PRINT_METHOD
    AST_VALIDATE_METHOD
    CODEGEN_METHOD
public:
    std::string id;
};

}
