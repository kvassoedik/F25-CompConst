#pragma once

#include "lexer/Token.h"
#include <string>
#include <vector>
#include <memory>
#include <unordered_map>

namespace Ast {
    struct Entity {
        Tokens::Span span;

        // virtual void generate() = 0;
        // virtual bool validate() = 0;
        void generate() {};
        bool validate() {return true;};
    };

    struct Decl : public Entity {
        Decl(std::string id)
            : id(std::move(id)) {}

        std::string id;
    };

    struct Expr : public Entity {
        std::shared_ptr<Expr> next;
    };
    struct Stmt : public Entity {};
    
    enum class TypeEnum {
        Int,
        Real,
        Bool,

        Array,
        Record,
    };

    struct Type : public Entity {
        Type(TypeEnum code)
            : code(code) {}

        TypeEnum code;
    };

    struct TypeDecl final : public Decl {
        std::shared_ptr<Type> type;
    };

    struct Block final : public Entity {
        std::vector<std::shared_ptr<Decl>> declarations;
        std::vector<std::shared_ptr<Stmt>> statements;
        std::unordered_map<std::string, std::shared_ptr<Decl>> declMap;
        std::unordered_map<std::string, std::shared_ptr<Type>> typeMap;
        std::shared_ptr<Block> parent{nullptr};

        // void generate() override;
        // bool validate() override;
    };

    struct RangeSpecifier : public Entity {};
    struct IntRange : public RangeSpecifier {
        std::shared_ptr<Expr> start;
        std::shared_ptr<Expr> end;
    };
    struct IdRef : public Expr, public RangeSpecifier {
        std::shared_ptr<IdRef> next;
    };
    
    /************************************ Statement ************************************/

    struct PrintStmt final : public Stmt {
        std::vector<std::shared_ptr<Expr>> args;
    };

    struct IfStmt final : public Stmt {
        std::shared_ptr<Expr> condition;
        Block body;
        std::shared_ptr<Block> elseBody;
    };
    
    struct WhileStmt final : public Stmt {
        std::shared_ptr<Expr> condition;
        Block body;
    };
    
    struct ForStmt final : public Stmt {
        std::string counterId;
        std::shared_ptr<RangeSpecifier> range;
        Block body;
        bool reverse;
    };

    struct ReturnStmt final : public Stmt {
        std::shared_ptr<IdRef> val;
    };

    /************************************ Var ************************************/

    struct VarDecl final : public Decl {
        VarDecl(std::string id, std::shared_ptr<Type> type)
            : Decl(std::move(id)), type(std::move(type)) {}

        std::shared_ptr<Type> type;

        // void generate() override;
        // bool validate() override;
    };

    struct Var final : public Entity {
        VarDecl header;
        std::shared_ptr<Expr> val;
    };

    struct Literal : public Entity {
        Literal(TypeEnum code)
            : code(code) {}

        const TypeEnum code;
    };
    struct RealLiteral final : public Literal {
        RealLiteral()
            : Literal(TypeEnum::Real) {}

        double val;
    };
    struct IntLiteral final : public Literal {
        IntLiteral()
            : Literal(TypeEnum::Int) {}

        long val;
    };
    struct BoolLiteral final : public Literal {
        BoolLiteral()
            : Literal(TypeEnum::Bool) {}

        bool val;
    };

    /************************************ Routine ************************************/

    struct RoutineDecl final : public Decl {
        RoutineDecl(std::string id)
            : Decl(std::move(id)) {}

        std::vector<std::shared_ptr<VarDecl>> params;
        std::shared_ptr<Type> retType;

        // void generate() override;
        // bool validate() override;
    };

    struct Routine final : public Entity {
        RoutineDecl header;
        Block body;
    };

    struct RoutineCall final : public IdRef {
        std::vector<std::shared_ptr<Expr>> args;
    };

    /************************************ Array ************************************/

    struct ArrayType final: public Type {
        ArrayType()
            : Type(TypeEnum::Array) {}

        std::shared_ptr<Expr> size;
        std::shared_ptr<Type> elemType;
    };

    struct IndexAccess final : public IdRef {
        std::shared_ptr<Expr> val;
    };

    /************************************ Record ************************************/

    struct RecordType final : public Type {
        RecordType()
            : Type(TypeEnum::Record) {}

        std::vector<std::shared_ptr<Var>> members;
    };

    struct NameAccess final : public IdRef {
        std::string_view id;
    };
}
