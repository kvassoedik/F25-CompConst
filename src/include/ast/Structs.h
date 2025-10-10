#pragma once

#include "ast/Entity.h"
#include "ast/DebugTree.h"
#include "utils/PrintingUtils.h"
#include <vector>
#include <memory>
#include <unordered_map>
#include <algorithm>

namespace Ast {

struct Decl : public Entity {
    Decl(Tokens::Span span, std::string id)
        : Entity(span), id(std::move(id)) {}

    AST_DEBUG_PRINT_METHOD_SIGNATURE override {};
public:
    std::string id;
};

struct Stmt : public Entity {
    Stmt(Tokens::Span span)
        : Entity(span) {}

    AST_DEBUG_PRINT_METHOD_SIGNATURE override {};
};

enum class TypeEnum {
    ERROR,
    RESOLVABLE,

    Int,
    Real,
    Bool,

    Array,
    Record,
};

struct Type : public Entity {
    Type(Tokens::Span span, TypeEnum code)
        : Entity(span), code(code) {}

    AST_DEBUG_PRINT_METHOD("Type_" << static_cast<int>(code))
public:
    TypeEnum code;
};

struct TypeRef final : public Type {
    TypeRef(Tokens::Span span, std::string id)
        : Type(span, TypeEnum::RESOLVABLE), id(std::move(id)) {}
    
    AST_DEBUG_PRINT_METHOD("TypeRef " << id)
public:
    std::string id;
};

struct TypeDecl final : public Decl {
    AST_DEBUG_PRINT_METHOD("TypeDecl " << id << " is " << AST_DEBUG_PTR_TO_STR(type))
public:
    std::shared_ptr<Type> type;
};

struct Block final : public Entity {
    Block(Tokens::Span span)
        : Entity(span) {}

    AST_DEBUG_PRINT_METHOD("Block { parent " << AST_DEBUG_PTR_TO_STR(parent) << ", decls (" << "}")

public:
    std::vector<std::shared_ptr<Decl>> declarations;
    std::vector<std::shared_ptr<Stmt>> statements;
    std::unordered_map<std::string, std::shared_ptr<Decl>> declMap;
    std::unordered_map<std::string, std::shared_ptr<Type>> typeMap;
    std::shared_ptr<Block> parent{nullptr};

    // void generate() override;
    // bool validate() override;
};

/************************************ Expr ************************************/

enum class ExprEnum {
    ERROR,

    BoolLiteral,
    IntLiteral,
    RealLiteral,

    CompoundPrimary,
    IdRef,
    ArrayAccess,

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

    std::shared_ptr<Type> type;
    ExprEnum code;
};

struct RangeSpecifier : public Entity {
    RangeSpecifier(Tokens::Span span)
        : Entity(span) {}
};
struct IntRange : public RangeSpecifier {
    IntRange(Tokens::Span span, std::shared_ptr<Expr> start, std::shared_ptr<Expr> end)
        : RangeSpecifier(span), start(std::move(start)), end(std::move(end)) {}

    std::shared_ptr<Expr> start;
    std::shared_ptr<Expr> end;
};

struct ModifiablePrimary : public Expr {
    ModifiablePrimary(Tokens::Span span)
        : Expr(span) {}

    std::shared_ptr<ModifiablePrimary> next;
};

struct IdRef : public ModifiablePrimary {
    IdRef(Tokens::Span span, std::string id)
        : ModifiablePrimary(span), id(std::move(id)) {
            code = ExprEnum::IdRef;
        }

    std::string id;
};

struct BoolLiteral : public Expr {
    BoolLiteral(Tokens::Span span, bool val)
        : Expr(span, ExprEnum::BoolLiteral), val(val) {}

    bool val;
};
struct IntLiteral : public Expr {
    IntLiteral(Tokens::Span span, long val)
        : Expr(span, ExprEnum::BoolLiteral), val(val) {}

    long val;
};
struct RealLiteral : public Expr {
    RealLiteral(Tokens::Span span, double val)
        : Expr(span, ExprEnum::BoolLiteral), val(val) {}

    double val;
};

struct ExprOperation : public Expr {
    ExprOperation(Tokens::Span span, ExprEnum code)
        : Expr(span, code) {}

    std::shared_ptr<Expr> left;
    std::shared_ptr<Expr> right;
};

/************************************ Statement ************************************/

struct PrintStmt final : public Stmt {
    PrintStmt(Tokens::Span span)
        : Stmt(span) {}

    std::vector<std::shared_ptr<Expr>> args;
};

struct IfStmt final : public Stmt {
    IfStmt(Tokens::Span span)
        : Stmt(span) {}

    std::shared_ptr<Expr> condition;
    std::shared_ptr<Block> body;
    std::shared_ptr<Block> elseBody;
};

struct WhileStmt final : public Stmt {
    WhileStmt(Tokens::Span span)
        : Stmt(span) {}

    std::shared_ptr<Expr> condition;
    std::shared_ptr<Block> body;
};

struct ForStmt final : public Stmt {
    ForStmt(Tokens::Span span)
        : Stmt(span) {}

    std::string counterId;
    std::shared_ptr<RangeSpecifier> range;
    std::shared_ptr<Block> body;
    bool reverse;
};

struct ReturnStmt final : public Stmt {
    ReturnStmt(Tokens::Span span)
        : Stmt(span) {}

    std::shared_ptr<Expr> val;
};

struct Assignment final : public Stmt {
    Assignment(Tokens::Span span)
        : Stmt(span) {}

    std::shared_ptr<ModifiablePrimary> left;
    std::shared_ptr<Expr> val;
};

/************************************ Var ************************************/

struct Var final : public Decl {
    Var(Tokens::Span span, std::string id)
        : Decl(span, std::move(id)) {}

    AST_DEBUG_PRINT_METHOD("var " << id << ": " << AST_DEBUG_PTR_TO_STR(type) << " is " << AST_DEBUG_PTR_TO_STR(val))

    std::shared_ptr<Type> type{nullptr};
    std::shared_ptr<Expr> val{nullptr};
};

/************************************ Routine ************************************/

struct Routine final : public Decl {
    Routine(Tokens::Span span, std::string id)
        : Decl(span, std::move(id)) {}

    AST_DEBUG_PRINT_METHOD_SIGNATURE override {
        std::string s;

        for (size_t i = 0; i < params.size(); ++i) {
            s += AST_DEBUG_PTR_TO_STR(params[i]);
            if (i+1 < params.size())
                s += ", ";
        }
        os << "routine " << id << "(" << s << "): " << AST_DEBUG_PTR_TO_STR(retType) << " is "
            << AST_DEBUG_PTR_TO_STR(body) << "   " << this->span << "\n";
    }
public:
    std::vector<std::shared_ptr<Var>> params;
    std::shared_ptr<Type> retType{nullptr};
    std::shared_ptr<Block> body{nullptr};
};

struct RoutineCall final : public Expr {
    RoutineCall(Tokens::Span span, std::shared_ptr<ModifiablePrimary> routineId)
        : Expr(span, ExprEnum::RoutineCall), routineId(std::move(routineId)) {}

    std::shared_ptr<ModifiablePrimary> routineId;
    std::vector<std::shared_ptr<Expr>> args;
};

/************************************ Array ************************************/

struct ArrayType final: public Type {
    ArrayType(Tokens::Span span)
        : Type(span, TypeEnum::Array) {}

    AST_DEBUG_PRINT_METHOD("array[ " << AST_DEBUG_PTR_TO_STR(size) << " ]: " << AST_DEBUG_PTR_TO_STR(elemType))
    
public:
    std::shared_ptr<Expr> size{nullptr};
    std::shared_ptr<Type> elemType{nullptr};
};

struct ArrayAccess final : public ModifiablePrimary {
    ArrayAccess(Tokens::Span span, std::shared_ptr<Expr> val)
        : ModifiablePrimary(span), val(std::move(val)) {
            code = ExprEnum::ArrayAccess;
        }

public:
    std::shared_ptr<Expr> val;
    std::shared_ptr<Expr> next;
};

/************************************ Record ************************************/

struct RecordType final : public Type {
    RecordType(Tokens::Span span)
        : Type(span, TypeEnum::Record) {}

    AST_DEBUG_PRINT_METHOD("record {" << "}")
public:
    std::vector<std::shared_ptr<Var>> members;
};

}
