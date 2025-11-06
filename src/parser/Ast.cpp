#include "parser/Ast.h"
#include "parser/structs.h"

using namespace ast;

Ast::Ast() {
    auto nullSpan = Tokens::Span{1,0,0};
    baseTypes_.error = mk<Type>(nullSpan, TypeEnum::ERROR);
    baseTypes_.boolean = mk<Type>(nullSpan, TypeEnum::Bool);
    baseTypes_.integer = mk<Type>(nullSpan, TypeEnum::Int);
    baseTypes_.real = mk<Type>(nullSpan, TypeEnum::Real);

    defaultInitializers_.boolean = mk<BoolLiteral>(nullSpan, false);
    defaultInitializers_.integer = mk<IntLiteral>(nullSpan, 0);
    defaultInitializers_.real = mk<RealLiteral>(nullSpan, 0.0);

    root_ = mk<Block>(nullSpan);
}

