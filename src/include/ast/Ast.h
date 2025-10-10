#pragma once

#include "ast/Structs.h"
#include "ast/DebugTree.h"

namespace Ast {

template <typename T, typename ...Args>
std::shared_ptr<T> mk(Args&&... args) {
    static_assert(std::is_base_of<Entity, T>::value, "T must be derived from Entity");

    std::shared_ptr<T> node = std::make_shared<T>(std::forward<Args>(args)...);
#if AST_DEBUG_ON
    Ast::debugInfo.newNode(node);
#endif

    return node;
}

}