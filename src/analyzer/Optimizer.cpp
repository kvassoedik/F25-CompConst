#include "parser/Parser.h"
#include "analyzer/Optimizer.h"
#include "analyzer/utils.h"
#include <functional>

using namespace analyzer;
using namespace Ast;
using std::shared_ptr;

int Optimizer::configure(int* argc, char** argv) {
    for (int i = 1; i < *argc-1; ++i) {
        std::string_view arg(argv[i]);

        if (arg.size() > 1 &&
            "-O" == arg.substr(0, 2))
        {
            // Messages
            std::string_view option = arg.size() > 2 ? arg.substr(2) : "";
            if (!option.empty() && 'V' == option[0]) {
                std::string_view s = option.size() > 1 ? option.substr(1) : "";
                if ("comput" == s)
                    config_.logs.computations = true;
                else if ("unused" == s)
                    config_.logs.unusedDecls = true;
                else {
                    std::cerr << "Unrecognized -OV option: " << s << "\n";
                    return 1;
                }
            } else if ("off" == option) {
                config_.disabled = true;
            } else {
                std::cerr << "Unrecognized -O option: " << option << "\n";
                return 1;
            }
        }
    }
    return 0;
}

void Optimizer::log(const Log& log) {
    std::cout << "#[Optimizer] " << log.msg << std::string(5, ' ') << ANSI_START ANSI_BOLD ANSI_APPLY
        << file_->fileName() << ":" << log.span.line
        << ":" << log.span.start - file_->lineStarts[log.span.line-1] + 1
        << ANSI_RESET << "\n";
}

shared_ptr<Expr> Optimizer::computeExpr(Expr& expr) {
// ------------------------ MACROS -----------------------------
#define spc std::static_pointer_cast
#define INT_REAL_CASE(_node_) \
(_node_->code == ExprEnum::IntLiteral ? spc<IntLiteral>(_node_)->val : spc<RealLiteral>(_node_)->val)
// -----------------------------------------------------

    if (isErrorType(expr.type) || config_.disabled)
        return nullptr;

    struct NumericBinaryOperation {
        std::function< decltype(IntLiteral::val) (shared_ptr<Expr> a, shared_ptr<Expr> b) > integer;
        std::function< decltype(RealLiteral::val) (shared_ptr<Expr> a, shared_ptr<Expr> b) > real;
    };
    auto optimizeNumericBinary = [&expr, this](NumericBinaryOperation operation) -> shared_ptr<Expr> {
        auto e = static_cast<BinaryExpr&>(expr);
        auto left = e.left->knownPrimitive ? e.left : computeExpr(*e.left);
        auto right = e.right->knownPrimitive ? e.right : computeExpr(*e.right);
        if (left && right) {
            if (e.type->code == TypeEnum::Int) {
                auto res = mk<IntLiteral>(
                    Tokens::Span{e.span.line, e.span.start, e.span.start+1},
                    operation.integer(left, right)
                );
                res->type = parser_.getBaseTypes().integer;
#if AST_DEBUG_ON
                res->optimized = true;
#endif
                if (config_.logs.computations)
                    log({"result"
#if AST_DEBUG_ON
                        + std::string(" [") + std::to_string(res->debugId) + "]"
#endif
                        + std::string(": ") + std::to_string(res->val), e.span});

                return res;
            }

            auto res = mk<RealLiteral>(
                Tokens::Span{e.span.line, e.span.start, e.span.start+1},
                operation.real(left, right)
            );
            res->type = parser_.getBaseTypes().real;
#if AST_DEBUG_ON
            res->optimized = true;
#endif
            if (config_.logs.computations)
                log({"result"
#if AST_DEBUG_ON
                    + std::string(" [") + std::to_string(res->debugId) + "]"
#endif
                    + std::string(": ") + std::to_string(res->val), e.span});

            return res;
        } else {
            if (left) e.left = std::move(left);
            if (right) e.right = std::move(right);
            return nullptr;
        }
    };

    auto optimizeNumericComparison = [&expr, this](NumericBinaryOperation operation) -> shared_ptr<Expr> {
        auto e = static_cast<BinaryExpr&>(expr);
        auto left = e.left->knownPrimitive ? e.left : computeExpr(*e.left);
        auto right = e.right->knownPrimitive ? e.right : computeExpr(*e.right);
        if (left && right) {
            auto res = mk<BoolLiteral>(
                Tokens::Span{e.span.line, e.span.start, e.span.start+1},
                e.type->code == TypeEnum::Int
                    ? operation.integer(left, right)
                    : operation.real(left, right)
            );
            res->type = parser_.getBaseTypes().boolean;
#if AST_DEBUG_ON
            res->optimized = true;
#endif
            if (config_.logs.computations)
                log({"result"
#if AST_DEBUG_ON
                    + std::string(" [") + std::to_string(res->debugId) + "]"
#endif
                    + std::string(": ") + (res->val ? "true" : "false"), e.span});

            return res;
        } else {
            if (left) e.left = std::move(left);
            if (right) e.right = std::move(right);
            return nullptr;
        }
    };

    auto optimizeBoolBinary = [&expr, this](std::function<
        bool (shared_ptr<BoolLiteral> a, shared_ptr<BoolLiteral> b)>
        operation
    ) -> shared_ptr<Expr> {
        auto e = static_cast<BinaryExpr&>(expr);
        auto left = e.left->knownPrimitive ? e.left : computeExpr(*e.left);
        auto right = e.right->knownPrimitive ? e.right : computeExpr(*e.right);
        if (left && right) {
            auto res = mk<BoolLiteral>(
                Tokens::Span{e.span.line, e.span.start, e.span.start+1},
                operation(
                    std::static_pointer_cast<BoolLiteral>(left),
                    std::static_pointer_cast<BoolLiteral>(right)
                )
            );
            res->type = parser_.getBaseTypes().boolean;
#if AST_DEBUG_ON
            res->optimized = true;
#endif
            if (config_.logs.computations)
                log({"result"
#if AST_DEBUG_ON
                    + std::string(" [") + std::to_string(res->debugId) + "]"
#endif
                    + std::string(": ") + (res->val ? "true" : "false"), e.span});

            return res;
        } else {
            if (left) e.left = std::move(left);
            if (right) e.right = std::move(right);
            return nullptr;
        }
    };

    auto areValuesEqual = [](BinaryExpr& e, shared_ptr<Expr> left, shared_ptr<Expr> right) {
        switch (e.type->code) {
        case TypeEnum::Bool: {
            return spc<BoolLiteral>(left)->val == spc<BoolLiteral>(right)->val;
        }
        case TypeEnum::Int: {
            return spc<IntLiteral>(left)->val == spc<IntLiteral>(right)->val;
        }
        case TypeEnum::Real: {
            return INT_REAL_CASE(left) == INT_REAL_CASE(right);
        }
        default:
            throw std::runtime_error(
                "Optimizer::optimizeExpr: unsupported compile-time equality type check: " + stringifyType(e.type)
            );
        }
    };

    switch (expr.code) {
    case ExprEnum::IdRef: {
        auto ref = std::static_pointer_cast<Var>(
            static_cast<IdRef&>(expr).ref.lock()
        );
        if (!ref || !ref->knownPrimitive)
            break;
        return ref->val;
    }
    case ExprEnum::Negate: {
        auto e = static_cast<UnaryExpr&>(expr);

        // primitives are optimized away at Parser stage
        auto opt = e.val->knownPrimitive ? e.val : computeExpr(*e.val);
        if (!opt)
            break;

        if (opt->type->code == TypeEnum::Int) {
            auto res = mk<IntLiteral>(
                Tokens::Span{e.span.line, e.span.start, e.span.start+1},
                -INT_REAL_CASE(opt)
            );
            res->type = parser_.getBaseTypes().integer;
#if AST_DEBUG_ON
            res->optimized = true;
#endif
            if (config_.logs.computations)
                log({"computed"
#if AST_DEBUG_ON
                        + std::string(" [") + std::to_string(res->debugId) + "]"
#endif
                        + std::string(": ") + std::to_string(res->val), e.span});

            return res;
        }

        auto res = mk<RealLiteral>(
            Tokens::Span{e.span.line, e.span.start, e.span.start+1},
            -INT_REAL_CASE(opt)
        );
        res->type = parser_.getBaseTypes().real;
#if AST_DEBUG_ON
        res->optimized = true;
#endif
        if (config_.logs.computations)
                log({"computed"
#if AST_DEBUG_ON
                        + std::string(" [") + std::to_string(res->debugId) + "]"
#endif
                        + std::string(": ") + std::to_string(res->val), e.span});

        return res;
    }
    case ExprEnum::Not: {
        auto e = static_cast<UnaryExpr&>(expr);
        auto opt = e.val->knownPrimitive ? e.val : computeExpr(*e.val);
        if (!opt)
            break;

        auto res = mk<BoolLiteral>(
            Tokens::Span{e.span.line, e.span.start, e.span.start+1},
            !std::static_pointer_cast<BoolLiteral>(opt)->val
        );
        res->type = parser_.getBaseTypes().boolean;
#if AST_DEBUG_ON
        res->optimized = true;
#endif
        if (config_.logs.computations)
                log({"computed"
#if AST_DEBUG_ON
                        + std::string(" [") + std::to_string(res->debugId) + "]"
#endif
                        + std::string(": not ") + std::to_string(std::static_pointer_cast<BoolLiteral>(opt)->val), e.span});

        return res;
    }
    case ExprEnum::Add: { auto op = [this](shared_ptr<Expr> a, shared_ptr<Expr> b) {
        if (config_.logs.computations)
            log({"computing " + std::to_string(INT_REAL_CASE(a)) + " + " + std::to_string(INT_REAL_CASE(b)), a->span});
        return INT_REAL_CASE(a) + INT_REAL_CASE(b);
        };
        return optimizeNumericBinary({op, op});
    }
    case ExprEnum::Subtract: { auto op = [this](shared_ptr<Expr> a, shared_ptr<Expr> b) {
        if (config_.logs.computations)
            log({"computing " + std::to_string(INT_REAL_CASE(a)) + " - " + std::to_string(INT_REAL_CASE(b)), a->span});
        return INT_REAL_CASE(a) - INT_REAL_CASE(b);
        };
        return optimizeNumericBinary({op, op});
    }
    case ExprEnum::Multiply: { auto op = [this](shared_ptr<Expr> a, shared_ptr<Expr> b) {
        if (config_.logs.computations)
            log({"computing " + std::to_string(INT_REAL_CASE(a)) + " * " + std::to_string(INT_REAL_CASE(b)), a->span});
        return INT_REAL_CASE(a) * INT_REAL_CASE(b);
        };
        return optimizeNumericBinary({op, op});
    }
    case ExprEnum::Divide: { auto op = [this](shared_ptr<Expr> a, shared_ptr<Expr> b) {
        if (config_.logs.computations)
            log({"computing " + std::to_string(INT_REAL_CASE(a)) + " / " + std::to_string(INT_REAL_CASE(b)), a->span});
        return INT_REAL_CASE(a) / INT_REAL_CASE(b);
        };
        return optimizeNumericBinary({op, op});
    }
    case ExprEnum::Modulo: { auto op = [this](shared_ptr<Expr> a, shared_ptr<Expr> b) {
        if (config_.logs.computations)
            log({"computing " + std::to_string(spc<IntLiteral>(a)->val) + " % " + std::to_string(spc<IntLiteral>(b)->val), a->span});
        return spc<IntLiteral>(a)->val % spc<IntLiteral>(b)->val;
        };
        return optimizeNumericBinary({op, nullptr});
    }
    case ExprEnum::LESS_THAN: { auto op = [this](shared_ptr<Expr> a, shared_ptr<Expr> b) {
        if (config_.logs.computations)
            log({"computing " + std::to_string(INT_REAL_CASE(a)) + " < " + std::to_string(INT_REAL_CASE(b)), a->span});
        return INT_REAL_CASE(a) < INT_REAL_CASE(b);
        };
        return optimizeNumericComparison({op, op});
    }
    case ExprEnum::LESS_OR_EQUAL: { auto op = [this](shared_ptr<Expr> a, shared_ptr<Expr> b) {
        if (config_.logs.computations)
            log({"computing " + std::to_string(INT_REAL_CASE(a)) + " <= " + std::to_string(INT_REAL_CASE(b)), a->span});
        return INT_REAL_CASE(a) <= INT_REAL_CASE(b);
        };
        return optimizeNumericComparison({op, op});
    }
    case ExprEnum::MORE_THAN: { auto op = [this](shared_ptr<Expr> a, shared_ptr<Expr> b) {
        if (config_.logs.computations)
            log({"computing " + std::to_string(INT_REAL_CASE(a)) + " > " + std::to_string(INT_REAL_CASE(b)), a->span});
        return INT_REAL_CASE(a) > INT_REAL_CASE(b);
        };
        return optimizeNumericComparison({op, op});
    }
    case ExprEnum::MORE_OR_EQUAL: { auto op = [this](shared_ptr<Expr> a, shared_ptr<Expr> b) {
        if (config_.logs.computations)
            log({"computing " + std::to_string(INT_REAL_CASE(a)) + " >= " + std::to_string(INT_REAL_CASE(b)), a->span});
        return INT_REAL_CASE(a) >= INT_REAL_CASE(b);
        };
        return optimizeNumericComparison({op, op});
    }
    case ExprEnum::And: { auto op = [this](shared_ptr<BoolLiteral> a, shared_ptr<BoolLiteral> b) {
        if (config_.logs.computations)
            log({"computing " + std::string(a->val ? "true" : "false") + " and " + (b->val ? "true" : "false"), a->span});
        return a->val && b->val;
        };
        return optimizeBoolBinary(op);
    }
    case ExprEnum::Or: { auto op = [this](shared_ptr<BoolLiteral> a, shared_ptr<BoolLiteral> b) {
        if (config_.logs.computations)
            log({"computing " + std::string(a->val ? "true" : "false") + " or " + (b->val ? "true" : "false"), a->span});
        return a->val || b->val;
        };
        return optimizeBoolBinary(op);
    }
    case ExprEnum::Xor: { auto op = [this](shared_ptr<BoolLiteral> a, shared_ptr<BoolLiteral> b) {
        if (config_.logs.computations)
            log({"computing " + std::string(a->val ? "true" : "false") + " xor " + (b->val ? "true" : "false"), a->span});
        return a->val ^ b->val;
        };
        return optimizeBoolBinary(op);
    }
    case ExprEnum::EQUAL: {
        auto e = static_cast<BinaryExpr&>(expr);
        auto left = e.left->knownPrimitive ? e.left : computeExpr(*e.left);
        auto right = e.right->knownPrimitive ? e.right : computeExpr(*e.right);
        if (left && right) {
            if (config_.logs.computations) {
                if (e.type->code == TypeEnum::Bool)
                    log({"computing " + std::string(left ? "true" : "false") + " == " + (right ? "true" : "false"), e.span});
                else
                    log({"computing " + std::to_string(INT_REAL_CASE(left)) + " == " + std::to_string(INT_REAL_CASE(right)), e.span});
            }

            auto res = mk<BoolLiteral>(
                Tokens::Span{e.span.line, e.span.start, e.span.start+1},
                areValuesEqual(e, left, right)
            );
            res->type = parser_.getBaseTypes().boolean;
#if AST_DEBUG_ON
            res->optimized = true;
#endif
            if (config_.logs.computations)
                log({"result"
#if AST_DEBUG_ON
                    + std::string(" [") + std::to_string(res->debugId) + "]"
#endif
                    + std::string(": ") + (res->val ? "true" : "false"), e.span});

            return res;
        } else {
            if (left) e.left = std::move(left);
            if (right) e.right = std::move(right);
            return nullptr;
        }
    }
    case ExprEnum::UNEQUAL: {
        auto e = static_cast<BinaryExpr&>(expr);
        auto left = e.left->knownPrimitive ? e.left : computeExpr(*e.left);
        auto right = e.right->knownPrimitive ? e.right : computeExpr(*e.right);
        if (left && right) {
            if (config_.logs.computations) {
                if (e.type->code == TypeEnum::Bool)
                    log({"computing " + std::string(left ? "true" : "false") + " /= " + (right ? "true" : "false"), e.span});
                else
                    log({"computing " + std::to_string(INT_REAL_CASE(left)) + " /= " + std::to_string(INT_REAL_CASE(right)), e.span});
            }

            auto res = mk<BoolLiteral>(
                Tokens::Span{e.span.line, e.span.start, e.span.start+1},
                !areValuesEqual(e, left, right)
            );
            res->type = parser_.getBaseTypes().boolean;
#if AST_DEBUG_ON
            res->optimized = true;
#endif
            if (config_.logs.computations)
                log({"result"
#if AST_DEBUG_ON
                    + std::string(" [") + std::to_string(res->debugId) + "]"
#endif
                    + std::string(": ") + (res->val ? "true" : "false"), e.span});

            return res;
        } else {
            if (left) e.left = std::move(left);
            if (right) e.right = std::move(right);
            return nullptr;
        }
    }
    }
    return nullptr;

#undef spc
#undef INT_REAL_CASE
}

void Optimizer::onBlockFinish(Ast::Block& currBlock) {
    // Removing redundant statements
    std::vector<std::list<std::shared_ptr<Entity>>::const_iterator> rm;
    for (auto it = currBlock.units.cbegin(); it != currBlock.units.end(); ++it) {
        for (auto p: unitsToBeRemoved_) {
            if (it->get() != p) continue;
            rm.push_back(it);
        }
    }
    for (auto& it: rm)
        currBlock.units.erase(it);

    removeUnusedDecls(currBlock);
}

void Optimizer::removeUnitFromCurrBlockLater(const Entity& node) {
    unitsToBeRemoved_.push_back(&node);
}

void Optimizer::removeUnusedDecls(Block& currBlock) {
    if (config_.disabled)
        return;

    std::vector<const std::string*> rm;
    for (auto& decl: currBlock.declMap) {
        if (decl.second->everUsed) continue;

        auto it = std::find(currBlock.units.begin(), currBlock.units.end(), decl.second);
        if (it != currBlock.units.end())
            currBlock.units.erase(it);
        rm.push_back(&decl.first);

        if (config_.logs.unusedDecls)
            log({"unused decl: " + decl.first, decl.second->span});
    }

    for (auto& id: rm)
        currBlock.declMap.erase(*id);
}

Optimizer::AssignmentOptStatus Optimizer::optimizeAssignmentAway(Ast::Assignment& node) {
    auto parent = std::static_pointer_cast<IdRef>(node.left)->ref.lock();
    if (!parent)
        return AssignmentOptStatus::Skip;

    auto& var = static_cast<Var&>(*parent);
    if (var.knownPrimitive) {
        if (node.val->knownPrimitive) {
            var.val = node.val;
            unitsToBeRemoved_.push_back(&node);
            return AssignmentOptStatus::Success;
        }
        return AssignmentOptStatus::Fail;
    }
    return AssignmentOptStatus::Skip;
}