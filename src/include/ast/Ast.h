#pragma once

#include <string>
#include <vector>
#include <memory>

namespace Ast {
    struct Entity {
        virtual void generate() = 0;
        virtual bool validate() = 0;
    };

    struct Decl : public Entity {
        std::string id;
    };

    struct Expr : public Entity {};
    struct Stmt : public Entity {};
    
    struct Block final : public Entity {
        std::vector<std::unique_ptr<Decl>> declarations;
        std::vector<std::unique_ptr<Stmt>> statements;

        void generate() override;
        bool validate() override;
    };

    struct RangeSpecifier : public Entity {};
    struct IntRange : public RangeSpecifier {
        std::unique_ptr<Expr> start;
        std::unique_ptr<Expr> end;
    };
    struct MemberSelect : public Expr, public RangeSpecifier {
        std::unique_ptr<MemberSelect> next;
    };

    /************************************ Type ************************************/

    enum class TypeEnum {
        Int,
        Real,
        Bool,

        Array,
        Record,
    };

    struct Type : public Entity {
        TypeEnum code;
    };

    struct TypeDecl : public Decl {
        std::unique_ptr<Type> type;
    };

    /************************************ Statement ************************************/

    struct PrintStmt final : public Stmt {
        std::vector<std::unique_ptr<Expr>> args;
    };

    struct IfStmt final : public Stmt {
        std::unique_ptr<Expr> condition;
        Block body;
        std::unique_ptr<Block> elseBody;
    };
    
    struct WhileStmt final : public Stmt {
        std::unique_ptr<Expr> condition;
        Block body;
    };
    
    struct ForStmt final : public Stmt {
        std::string counterId;
        std::unique_ptr<RangeSpecifier> range;
        Block body;
        bool reverse;
    };

    struct ReturnStmt final : public Stmt {
        std::unique_ptr<MemberSelect> val;
    };

    /************************************ Var ************************************/

    struct VarDecl final : public Decl {
        std::unique_ptr<Type> type;

        void generate() override;
        bool validate() override;
    };

    struct Var final : public Entity {
        VarDecl header;
        std::unique_ptr<Expr> val;
    };

    /************************************ Routine ************************************/

    struct RoutineDecl : public Decl {
        std::vector<VarDecl> params;
        std::unique_ptr<Type> retType;

        void generate() override;
        bool validate() override;
    };

    struct Routine final : public Entity {
        RoutineDecl header;
        Block body;
    };

    struct RoutineCall final : public MemberSelect {
        std::vector<std::unique_ptr<Expr>> args;
    };

    /************************************ Array ************************************/

    struct ArrayType final: public Type {
        std::unique_ptr<Expr> size;
        std::unique_ptr<Type> elemType;
    };

    struct IndexAccess final : public MemberSelect {
        std::unique_ptr<Expr> val;
    };

    /************************************ Record ************************************/

    struct RecordType{
        std::vector<std::unique_ptr<Var>> members;
    };

    struct NameAccess final : public MemberSelect {
        std::string_view id;
    };
}
