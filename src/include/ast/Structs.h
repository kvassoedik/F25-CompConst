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

    AST_DEBUG_PRINT_METHOD_SIGNATURE override {
        std::string sUnits, sDecls, sTypes;

        for (size_t i = 0; i < units.size(); ++i) {
            sUnits += AST_DEBUG_PTR_TO_STR(units[i]);
            if (i+1 < units.size())
                sUnits += ", ";
        }

        for (auto&& it = declMap.begin(); it != declMap.end(); ++it) {
            sUnits += AST_DEBUG_PTR_TO_STR(it->second);
            sUnits += ", ";
        }
        if (!sUnits.empty()) {
            sUnits.pop_back();
            sUnits.pop_back();
        }

        for (auto&& it = typeMap.begin(); it != typeMap.end(); ++it) {
            sTypes += AST_DEBUG_PTR_TO_STR(it->second);
            sTypes += ", ";
        }
        if (!sTypes.empty()) {
            sTypes.pop_back();
            sTypes.pop_back();
        }
        
        std::string pd(2, ' ');
        os << "Block {"
            << newline << pd << "parent " << AST_DEBUG_PTR_TO_STR(parent)
            << newline << pd << "units: " << sUnits
            << newline << pd << "decls: " << sDecls
            << newline << pd << "types: " << sTypes
            << newline << "}" << AST_DEBUG_PRINT_METHOD_IMPL_TAIL;
    }

public:
    std::vector<std::shared_ptr<Entity>> units;
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

    Negate,
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

    AST_DEBUG_PRINT_METHOD("Expr_" << static_cast<int>(code));
public:
    std::shared_ptr<Type> type{nullptr};
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

    AST_DEBUG_PRINT_METHOD("IdRef " << id)
public:
    std::string id;
};

struct BoolLiteral : public Expr {
    BoolLiteral(Tokens::Span span, bool val)
        : Expr(span, ExprEnum::BoolLiteral), val(val) {}

    AST_DEBUG_PRINT_METHOD((val ? "true" : "false"))
public:
    bool val;
};
struct IntLiteral : public Expr {
    IntLiteral(Tokens::Span span, long val)
        : Expr(span, ExprEnum::IntLiteral), val(val) {}

    AST_DEBUG_PRINT_METHOD("int " << val)
public:
    long val;
};
struct RealLiteral : public Expr {
    RealLiteral(Tokens::Span span, double val)
        : Expr(span, ExprEnum::RealLiteral), val(val) {}

    AST_DEBUG_PRINT_METHOD("real " << val)
public:
    double val;
};

struct BinaryExpr : public Expr {
    BinaryExpr(Tokens::Span span, ExprEnum code)
        : Expr(span, code) {}

    AST_DEBUG_PRINT_METHOD_SIGNATURE override {
        std::string operation;
        switch(code) {
            case ExprEnum::Add: {
                operation = "+";
                break;
            }
            case ExprEnum::Subtract: {
                operation = "-";
                break;
            }
            case ExprEnum::Multiply: {
                operation = "*";
                break;
            }
            case ExprEnum::Divide: {
                operation = "/";
                break;
            }
            case ExprEnum::Modulo: {
                operation = "%";
                break;
            }
            default: {
                operation = " INVALID_OPERATION_CODE ";
            }
        }

        os << AST_DEBUG_PTR_TO_STR(left) << operation << AST_DEBUG_PTR_TO_STR(right) << AST_DEBUG_PRINT_METHOD_IMPL_TAIL;
    }
public:
    std::shared_ptr<Expr> left{nullptr};
    std::shared_ptr<Expr> right{nullptr};
};
struct UnaryExpr : public Expr {
    UnaryExpr(Tokens::Span span, ExprEnum code)
        : Expr(span, code) {}

    AST_DEBUG_PRINT_METHOD("UnaryExpr_" << static_cast<int>(code) << " " << AST_DEBUG_PTR_TO_STR(val))
public:
    std::shared_ptr<Expr> val{nullptr};
};

/************************************ Statement ************************************/

struct PrintStmt final : public Entity {
    PrintStmt(Tokens::Span span)
        : Entity(span) {}

    AST_DEBUG_PRINT_METHOD_SIGNATURE override {
        std::string sArgs;

        for (size_t i = 0; i < args.size(); ++i) {
            sArgs += AST_DEBUG_PTR_TO_STR(args[i]);
            if (i+1 < args.size())
                sArgs += ", ";
        }
        os << "print " << sArgs << AST_DEBUG_PRINT_METHOD_IMPL_TAIL;
    }
public:
    std::vector<std::shared_ptr<Expr>> args;
};

struct IfStmt final : public Entity {
    IfStmt(Tokens::Span span)
        : Entity(span) {}

    AST_DEBUG_PRINT_METHOD("if " << AST_DEBUG_PTR_TO_STR(condition)
        << " then " << AST_DEBUG_PTR_TO_STR(body)
        << " else " << AST_DEBUG_PTR_TO_STR(elseBody)<< AST_DEBUG_PRINT_METHOD_IMPL_TAIL);
public:
    std::shared_ptr<Expr> condition{nullptr};
    std::shared_ptr<Block> body{nullptr};
    std::shared_ptr<Block> elseBody{nullptr};
};

struct WhileStmt final : public Entity {
    WhileStmt(Tokens::Span span)
        : Entity(span) {}

    std::shared_ptr<Expr> condition;
    std::shared_ptr<Block> body;
};

struct ForStmt final : public Entity {
    ForStmt(Tokens::Span span)
        : Entity(span) {}

    std::string counterId;
    std::shared_ptr<RangeSpecifier> range;
    std::shared_ptr<Block> body;
    bool reverse;
};

struct ReturnStmt final : public Entity {
    ReturnStmt(Tokens::Span span)
        : Entity(span) {}

    std::shared_ptr<Expr> val{nullptr};
};

struct Assignment final : public Entity {
    Assignment(Tokens::Span span)
        : Entity(span) {}

    AST_DEBUG_PRINT_METHOD(AST_DEBUG_PTR_TO_STR(left) << " := " << AST_DEBUG_PTR_TO_STR(val))
public:
    std::shared_ptr<ModifiablePrimary> left{nullptr};
    std::shared_ptr<Expr> val{nullptr};
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
        std::string sParams;

        for (size_t i = 0; i < params.size(); ++i) {
            sParams += AST_DEBUG_PTR_TO_STR(params[i]);
            if (i+1 < params.size())
                sParams += ", ";
        }
        os << "routine " << id << "(" << sParams << "): " << AST_DEBUG_PTR_TO_STR(retType) << " is "
            << AST_DEBUG_PTR_TO_STR(body) << AST_DEBUG_PRINT_METHOD_IMPL_TAIL;
    }
public:
    std::vector<std::shared_ptr<Var>> params;
    std::shared_ptr<Type> retType{nullptr};
    std::shared_ptr<Block> body{nullptr};
};

struct RoutineCall final : virtual public Expr {
    RoutineCall(Tokens::Span span, std::shared_ptr<ModifiablePrimary> routineId)
        : Expr(span, ExprEnum::RoutineCall), routineId(std::move(routineId)) {}

    AST_DEBUG_PRINT_METHOD_SIGNATURE override {
        std::string sArgs;

        for (size_t i = 0; i < args.size(); ++i) {
            sArgs += AST_DEBUG_PTR_TO_STR(args[i]);
            if (i+1 < args.size())
                sArgs += ", ";
        }
        os << "RoutineCall " << AST_DEBUG_PTR_TO_STR(routineId) << " (" << sArgs << ")" << AST_DEBUG_PRINT_METHOD_IMPL_TAIL;
    }
public:
    std::shared_ptr<ModifiablePrimary> routineId;
    std::vector<std::shared_ptr<Expr>> args;
};

/************************************ Array ************************************/

struct ArrayType final: public Type {
    ArrayType(Tokens::Span span)
        : Type(span, TypeEnum::Array) {}

    AST_DEBUG_PRINT_METHOD("array[" << AST_DEBUG_PTR_TO_STR(size) << "]: " << AST_DEBUG_PTR_TO_STR(elemType))
    
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
