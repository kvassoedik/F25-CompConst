#pragma once

#include "lexer/Token.h"
#include <string>
#include <vector>
#include <memory>
#include <unordered_map>

namespace Ast {
    struct Entity {
        Entity(Tokens::Span span)
            : span(span) {}

        Tokens::Span span;

        // virtual void generate() = 0;
        // virtual bool validate() = 0;
        void generate() {};
        bool validate() {return true;};
    };

    struct Decl : public Entity {
        Decl(Tokens::Span span, std::string id)
            : Entity(span), id(std::move(id)) {}

        std::string id;
    };

    struct Stmt : public Entity {};
    
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

        TypeEnum code;
    };

    struct TypeRef : public Type {
        TypeRef(Tokens::Span span, std::string id)
            : Type(span, TypeEnum::RESOLVABLE), id(std::move(id)) {}

        std::string id;
    };

    struct TypeDecl final : public Decl {
        std::shared_ptr<Type> type;
    };

    struct Block final : public Entity {
        Block(Tokens::Span span)
            : Entity(span) {}

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
        And,
        Or,
        Xor,
        RoutineCall,
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

    struct CompoundPrimary : public Expr {
        CompoundPrimary(Tokens::Span span)
            : Expr(span) {}

        std::shared_ptr<CompoundPrimary> next;
    };
    
    struct IdRef : public CompoundPrimary {
        IdRef(Tokens::Span span, std::string id)
            : CompoundPrimary(span), id(std::move(id)) {
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
        std::vector<std::shared_ptr<Expr>> args;
    };

    struct IfStmt final : public Stmt {
        std::shared_ptr<Expr> condition;
        std::shared_ptr<Block> body;
        std::shared_ptr<Block> elseBody;
    };
    
    struct WhileStmt final : public Stmt {
        std::shared_ptr<Expr> condition;
        std::shared_ptr<Block> body;
    };
    
    struct ForStmt final : public Stmt {
        std::string counterId;
        std::shared_ptr<RangeSpecifier> range;
        std::shared_ptr<Block> body;
        bool reverse;
    };

    struct ReturnStmt final : public Stmt {
        std::shared_ptr<Expr> val;
    };

    /************************************ Var ************************************/

    struct VarDecl final : public Decl {
        VarDecl(Tokens::Span span, std::string id, std::shared_ptr<Type> type)
            : Decl(span, std::move(id)), type(std::move(type)) {}

        std::shared_ptr<Type> type;

        // void generate() override;
        // bool validate() override;
    };

    struct Var final : public Entity {
        Var(Tokens::Span span, VarDecl header, std::shared_ptr<Expr> val)
            : Entity(span), header(header), val(val) {}

        VarDecl header;
        std::shared_ptr<Expr> val;
    };

    struct Literal : public Entity {
        Literal(Tokens::Span span, TypeEnum code)
            : Entity(span), code(code) {}

        const TypeEnum code;
    };
    struct RealLiteral final : public Literal {
        RealLiteral(Tokens::Span span)
            : Literal(span, TypeEnum::Real) {}

        double val;
    };
    struct IntLiteral final : public Literal {
        IntLiteral(Tokens::Span span)
            : Literal(span, TypeEnum::Int) {}

        long val;
    };
    struct BoolLiteral final : public Literal {
        BoolLiteral(Tokens::Span span)
            : Literal(span, TypeEnum::Bool) {}

        bool val;
    };

    /************************************ Routine ************************************/

    struct RoutineDecl final : public Decl {
        RoutineDecl(Tokens::Span span, std::string id)
            : Decl(span, std::move(id)) {}

        std::vector<std::shared_ptr<VarDecl>> params;
        std::shared_ptr<Type> retType;

        // void generate() override;
        // bool validate() override;
    };

    struct Routine final : public Entity {
        Routine(Tokens::Span span)
            : Entity(span) {}

        std::shared_ptr<RoutineDecl> header;
        std::shared_ptr<Block> body;
    };

    struct RoutineCall final : public Expr {
        RoutineCall(Tokens::Span span, std::shared_ptr<CompoundPrimary> routineId)
            : Expr(span, ExprEnum::RoutineCall), routineId(std::move(routineId)) {}

        std::shared_ptr<CompoundPrimary> routineId;
        std::vector<std::shared_ptr<Expr>> args;
    };

    /************************************ Array ************************************/

    struct ArrayType final: public Type {
        ArrayType(Tokens::Span span)
            : Type(span, TypeEnum::Array) {}

        std::shared_ptr<Expr> size;
        std::shared_ptr<Type> elemType;
    };

    struct ArrayAccess final : public CompoundPrimary {
        ArrayAccess(Tokens::Span span, std::shared_ptr<Expr> val)
            : CompoundPrimary(span), val(std::move(val)) {
                code = ExprEnum::ArrayAccess;
            }

        std::shared_ptr<Expr> val;
        std::shared_ptr<Expr> next;
    };

    /************************************ Record ************************************/

    struct RecordType final : public Type {
        RecordType(Tokens::Span span)
            : Type(span, TypeEnum::Record) {}

        std::vector<std::shared_ptr<Var>> members;
    };
}
