#pragma once

#include "parser/structs_fwd.h"
#include "parser/DebugTree.h"

namespace ast {

class Ast final {
public:
    Ast();

    template <typename T, typename ...Args>
    std::shared_ptr<T> mk(Args&&... args) {
        static_assert(std::is_base_of<Entity, T>::value, "T must be derived from Entity");

        std::shared_ptr<T> node = std::make_shared<T>(*this, std::forward<Args>(args)...);
#if AST_DEBUG_ON
        debugInfo.newNode(node);
#endif

        return node;
    }

    const std::shared_ptr<Block>& getRoot() const noexcept { return root_; }

    struct BaseTypes;
    const BaseTypes& getBaseTypes() const noexcept { return baseTypes_; }

    struct DefaultInitializers;
    const DefaultInitializers& getDefaultInitializers() const noexcept { return defaultInitializers_; };

private:
    struct BaseTypes {
        std::shared_ptr<Type> error, none, boolean, integer, real;
    } baseTypes_;

    struct DefaultInitializers {
        std::shared_ptr<BoolLiteral> boolean;
        std::shared_ptr<IntLiteral> integer;
        std::shared_ptr<RealLiteral> real;
    } defaultInitializers_;

    std::shared_ptr<Block> root_;
};

}