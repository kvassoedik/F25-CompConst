#include "ast/Parser.h"
#include <iostream>
#include <sstream>

#define ID_STR(tk) static_cast<Tokens::IdentifierTk*>(&*tk)->identifier

int Parser::configure(int* argc, char** argv) {
    return 0;
}

void Parser::feed(TokenList tokens) { tokens_ = std::move(tokens); }

void Parser::parse() {
    while (nextNode());
}

bool Parser::releaseErrors() {
    return reporter_.reportAll();
}

/*******************************************************************************************************************/

bool Parser::nextNode() {
    auto&& tk = tokens_.get();
    if (!tk) return false;

    startTk_ = std::move(tk);
    tokens_.move();
    
    switch (startTk_->type) {
    case TokenType::Routine: {
        auto&& node = parseRoutine();
        if (node) {
            currBlock_->declarations.emplace_back(node);
            currBlock_->declMap.emplace(node->header.id, std::move(node->header));
        }
        break;
    }
    
    }

    return true;
}

void Parser::saveError(Tokens::Span span, std::string reason) {
    reporter_.report({
        .level = CompileMsg::Level::Error,
        .message = std::move(reason),
        .span = span,
    });
}

std::shared_ptr<Ast::Decl> Parser::findDeclaration(const std::string& id) {
    Ast::Block *block = currBlock_.get();
    while (block) {
        auto&& it = block->declMap.find(id);
        if (it != block->declMap.end())
            return it->second;
            
        block = block->parent.get();
    }
    return {nullptr};
}

std::shared_ptr<Ast::Type> Parser::findNamedType(const std::string& id) {
    Ast::Block *block = currBlock_.get();
    while (block) {
        auto&& it = block->typeMap.find(id);
        if (it != block->typeMap.end())
            return it->second;
            
        block = block->parent.get();
    }
    return {nullptr};
}

std::shared_ptr<Ast::Block> Parser::parseBlock() {
    return nullptr;
}

std::shared_ptr<Ast::Routine> Parser::parseRoutine() {
    std::cout << "= Parsing routine\n";

    auto&& id = tokens_.get();
    if (!id || id->type != TokenType::Identifier) {
        std::stringstream ss;
        ss << "expected routine identifier, got ";
        saveError(id->span, ss.str());
        return nullptr;
    }
    tokens_.move();

    {
        // Previously defined?
        auto&& prevDef = currBlock_->declMap.find(ID_STR(id));
        if (prevDef != currBlock_->declMap.end()) {
            std::stringstream ss;
            ss << "redefinition of identifier " << ID_STR(id);
            saveError(id->span, ss.str());

            reporter_.report({
                .level = CompileMsg::Level::Appendix,
                .message = "previously defined here:",
                .span = prevDef->second->span,
            });
            return nullptr;
        }
    }

    auto&& res = std::make_shared<Ast::Routine>(id->span);
    auto&& routineDecl = std::make_shared<Ast::RoutineDecl>(id->span, ID_STR(id));
    std::shared_ptr<Ast::Block> body;

    auto&& tk = tokens_.get();
    if (!tk || (tk->type != TokenType::BRACKET_OPEN && tk->type != TokenType::ROUTINE_ARROW)) {
        saveError({
            .line = tk->span.line,
            .start = tk->span.start,
            .end = tk->span.start+1,
        }, "expected '(' or '=>' after routine identifier");
        return nullptr;
    }
    tokens_.move();

    if (tk->type == TokenType::BRACKET_OPEN) {
        // Parse params
        {
            std::shared_ptr<Tokens::BaseTk> nextParamTk;
            while ((nextParamTk = tokens_.get()) != nullptr) {
                tk = std::move(nextParamTk);
                if (tk->type == TokenType::BRACKET_CLOSE) {
                    break;
                } else {
                    tokens_.move();
                    auto&& param = parseRoutineParam();
                    
                    while ((nextParamTk = tokens_.get()) != nullptr) {
                        tk = std::move(nextParamTk);
                        if (tk->type == TokenType::BRACKET_CLOSE)
                            break;

                        tokens_.move();
                        if (tk->type == TokenType::COMMA)
                            break;
                    }
                }
            }
        }

        auto&& bracket = tokens_.get();
        if (!bracket || bracket->type != TokenType::BRACKET_CLOSE) {
            saveError({
                .line = tk->span.line,
                .start = tk->span.end,
                .end = tk->span.end + 1,
            }, "expected ')' after routine parameters");
            return nullptr;
        }

        if ((tk = tokens_.moveGet()) != nullptr) {
            if (tk->type == TokenType::Is) {
                tokens_.move();
                // TODO parse body
            } else if (tk->type != TokenType::ENDLINE || tk->type != TokenType::SEMICOLON) {
                saveError({
                    .line = tk->span.line,
                    .start = tk->span.start,
                    .end = tk->span.start + 1,
                }, "expected 'is' before routine body");
                return nullptr;
            }
        }
    } else {
        // TODO =>
    }

    res->header = std::move(routineDecl);
    res->body = std::move(body);
    return res;
}

std::shared_ptr<Ast::VarDecl> Parser::parseRoutineParam() {
    std::cout << "= Parsing param\n";

    auto&& id = tokens_.get();
    if (!id || id->type != TokenType::Identifier) {
        saveError(
            id ? id->span : Tokens::Span{file_->lineStarts.back(), file_->size(), file_->size()},
            "expected parameter identifier"
        );
        return nullptr;
    }
    
    std::shared_ptr<Tokens::BaseTk> tk = tokens_.moveGet();
    if (!tk || tk->type != TokenType::COLON) {
        saveError({
            .line = tk ? tk->span.line : id->span.line,
            .start = tk ? tk->span.start : id->span.end,
            .end = tk ? tk->span.end : id->span.end+1,
        }, "expected ':' after parameter identifier");
        return nullptr;
    }

    tokens_.move();
    auto&& type = parseType();

    return std::make_shared<Ast::VarDecl>(
        Tokens::Span{id->span.start, id->span.start, type->span.end},
        ID_STR(id),
        std::move(type)
    );
}

std::shared_ptr<Ast::Type> Parser::parseType() {
    std::cout << "= Parsing type\n";

    auto&& tk = tokens_.get();
    if (!tk)
        return std::make_shared<Ast::Type>(
            Tokens::Span{file_->lineStarts.back(), file_->size(), file_->size()},
            Ast::TypeEnum::ERROR
        );

    switch (tk->type)
    {
    case TokenType::IntegerType: return {nullptr, std::make_shared<Ast::Type>(tk->span, Ast::TypeEnum::Int)};
    case TokenType::RealType: return {nullptr, std::make_shared<Ast::Type>(tk->span, Ast::TypeEnum::Real)};
    case TokenType::BooleanType: return {nullptr, std::make_shared<Ast::Type>(tk->span, Ast::TypeEnum::Bool)};
    case TokenType::Identifier: return std::make_shared<Ast::TypeRef>(tk->span, ID_STR(tk));
    case TokenType::Array: {
        auto&& bracketOpen = tokens_.moveGet();
        if (!bracketOpen || bracketOpen->type != TokenType::SQUARE_BRACKET_OPEN) {
            unsigned long start = bracketOpen ? bracketOpen->span.start : tk->span.start+1;
            saveError(
                Tokens::Span{.line = tk->span.line, .start = start, .end = start+1},
                "expected '[' when declaring an array type"
            );

            return std::make_shared<Ast::Type>(
                Tokens::Span{tk->span.line, tk->span.start, bracketOpen ? bracketOpen->span.end : tk->span.end},
                Ast::TypeEnum::ERROR
            );
        }

        tokens_.move();
        auto&& expr = parseExpr();

        auto&& bracketClose = tokens_.get();
        if (!bracketClose || bracketClose->type != TokenType::SQUARE_BRACKET_CLOSE) {
            unsigned long start = bracketClose ? bracketClose->span.start : bracketOpen->span.start+1;
            saveError(
                Tokens::Span{tk->span.line, start, start+1},
                "expected ']' after expression when declaring an array type"
            );
            
            return std::make_shared<Ast::Type>(
                Tokens::Span{tk->span.line, tk->span.start, bracketClose ? bracketClose->span.end : bracketOpen->span.end},
                Ast::TypeEnum::ERROR
            );
        }

        tokens_.move();
        auto&& elemType = parseType();

        if (elemType->code == Ast::TypeEnum::ERROR) {
            // err
            return std::make_shared<Ast::Type>(
                Tokens::Span{tk->span.line, tk->span.start, elemType->span.end},
                Ast::TypeEnum::ERROR
            );
        }

        auto&& res = std::make_shared<Ast::ArrayType>(Tokens::Span{
            .line = tk->span.line,
            .start = tk->span.start,
            .end = elemType->span.end,
        });
        res->size = std::move(expr);
        res->elemType = std::move(elemType);

        return res;
    }
    case TokenType::Record: {
        auto&& res = std::make_shared<Ast::RecordType>(Tokens::Span{.line = tk->span.line, .start = tk->span.start});
        auto&& contents = tokens_.moveGet();
        while (contents) {
            if (contents->type == TokenType::End) {
                res->span.end = contents->span.end;
                return res;
            }

            auto&& var = parseVar();
            if (var)
                res->members.emplace_back(std::move(var));

            auto&& nextTk = tokens_.moveGet();
            if (!nextTk)
                break;
            contents = std::move(nextTk);
        }

        return std::make_shared<Ast::Type>(
            Tokens::Span{tk->span.line, tk->span.start, contents->span.end},
            Ast::TypeEnum::ERROR
        );
    }
    }
    
    return std::make_shared<Ast::Type>(tk->span, Ast::TypeEnum::ERROR);
}

std::shared_ptr<Ast::Var> Parser::parseVar() {
    auto&& tk = tokens_.get();
    if (!tk)
        return nullptr;

    if (tk->type != TokenType::Var) {
        saveError(tk->span, "Expected 'var' before new declaration");
        return nullptr;
    }

    auto&& id = tokens_.moveGet();
    if (!id || tk->type != TokenType::Identifier) {
        // err
        return nullptr;
    }

    Tokens::Span headerSpan{.line = tk->span.line, .start = tk->span.start, .end = id->span.end};
    std::shared_ptr<Ast::Type> explicitType{nullptr};

    auto&& colon = tokens_.moveGet();
    if (colon && colon->type == TokenType::COLON) {
        tokens_.move();
        explicitType = parseType();
        headerSpan.end = explicitType->span.end;
    }

    auto&& is = tokens_.moveGet();
    if (!is || is->type != TokenType::Is) {
        if (!explicitType) {
            // err
            return nullptr;
        }

        return std::make_shared<Ast::Var>(
            headerSpan,
            std::make_shared<Ast::VarDecl>(headerSpan, ID_STR(id), explicitType),
            nullptr
        );
    }

    auto&& expr = parseExpr();
    return std::make_shared<Ast::Var>(
        Tokens::Span{headerSpan.line, headerSpan.start, expr ? expr->span.end : headerSpan.end},
        std::make_shared<Ast::VarDecl>(
            headerSpan,
            ID_STR(id),
            explicitType || (expr ? expr->type : std::make_shared<Ast::Type>(headerSpan, Ast::TypeEnum::ERROR))
        ),
        expr
    );
}

/*******************************************************************************************************************/
/*************************************************** Expressions ***************************************************/
/*******************************************************************************************************************/

std::shared_ptr<Ast::Expr> Parser::parseExpr() {
    std::shared_ptr<Ast::Expr> left = parseRelation();
    if (!left)
        return nullptr;

    std::shared_ptr<Tokens::BaseTk> tk;
    while (((tk = tokens_.get()) != nullptr) && (
            tk->type == TokenType::And || tk->type == TokenType::Or
            || tk->type == TokenType::Xor
        )) {
        tokens_.move();
        auto&& right = parseRelation();
        if (!right) {
            // err
            break;
        }

        auto&& expr = std::make_shared<Ast::ExprOperation>(Tokens::Span{left->span.line, left->span.start, right->span.end});
        expr->left = std::move(left);
        expr->right = std::move(right);
        left = std::move(expr);
    }
    
    return left;
}

std::shared_ptr<Ast::Expr> Parser::parseRelation() {
    std::shared_ptr<Ast::Expr> left = parseSimple();
    if (!left)
        return nullptr;

    std::shared_ptr<Tokens::BaseTk> tk;
    while (((tk = tokens_.get()) != nullptr) && (
            tk->type == TokenType::LESS_THAN || tk->type == TokenType::LESS_OR_EQUAL
            || tk->type == TokenType::MORE_THAN || tk->type == TokenType::MORE_OR_EQUAL
            || tk->type == TokenType::EQUAL || tk->type == TokenType::UNEQUAL
        )) {
        tokens_.move();
        auto&& right = parseSimple();
        if (!right) {
            // err
            break;
        }

        auto&& relation = std::make_shared<Ast::ExprOperation>(Tokens::Span{left->span.line, left->span.start, right->span.end});
        relation->left = std::move(left);
        relation->right = std::move(right);
        left = std::move(relation);
    }
    
    return left;
}

std::shared_ptr<Ast::Expr> Parser::parseSimple() {
    std::shared_ptr<Ast::Expr> left = parseFactor();
    if (!left)
        return nullptr;

    std::shared_ptr<Tokens::BaseTk> tk;
    while (((tk = tokens_.get()) != nullptr) && (
        tk->type == TokenType::TIMES || tk->type == TokenType::DIVIDE || tk->type == TokenType::MODULO
    )) {
        tokens_.move();
        auto&& right = parseFactor();
        if (!right) {
            // err
            break;
        }

        auto&& simple = std::make_shared<Ast::ExprOperation>(Tokens::Span{left->span.line, left->span.start, right->span.end});
        simple->left = std::move(left);
        simple->right = std::move(right);
        left = std::move(simple);
    }
    
    return left;
}

std::shared_ptr<Ast::Expr> Parser::parseFactor() {
    std::shared_ptr<Ast::Expr> left = parseSummand();
    if (!left)
        return nullptr;

    std::shared_ptr<Tokens::BaseTk> tk;
    while (((tk = tokens_.get()) != nullptr) && (
        tk->type == TokenType::PLUS || tk->type == TokenType::MINUS
    )) {
        tokens_.move();
        auto&& right = parseSummand();
        if (!right) {
            // err
            break;
        }

        auto&& factor = std::make_shared<Ast::ExprOperation>(Tokens::Span{left->span.line, left->span.start, right->span.end});
        factor->left = std::move(left);
        factor->right = std::move(right);
        left = std::move(factor);
    }
    
    return left;
}

std::shared_ptr<Ast::Expr> Parser::parseSummand() {
    auto&& tk = tokens_.get();
    if (!tk)
        return nullptr;

    if (tk->type == TokenType::BRACKET_OPEN) {
        tokens_.move();
        auto&& expr = parseExpr();
        if (!expr) {
            // err
            return nullptr;
        }
        
        auto&& bracket = tokens_.get();
        if (!bracket || bracket->type != TokenType::BRACKET_CLOSE) {
            // err
            return nullptr;
        }

        tokens_.move();
        expr->span.line = tk->span.line;
        expr->span.start = tk->span.start;
        expr->span.end = bracket->span.end;
        return expr;
    }

    return parsePrimary();
}

std::shared_ptr<Ast::Expr> Parser::parsePrimary() {
    auto&& tk = tokens_.get();
    if (!tk)
        return nullptr;

    switch (tk->type) {
    case TokenType::True: {
        tokens_.move();
        return std::make_shared<Ast::BoolLiteral>(tk->span, true);
    }
    case TokenType::False: {
        tokens_.move();
        return std::make_shared<Ast::BoolLiteral>(tk->span, false);
    }
    case TokenType::IntLiteral: {
        tokens_.move();
        return std::make_shared<Ast::IntLiteral>(tk->span, static_cast<Tokens::IntTk*>(&*tk)->value);
    }
    case TokenType::RealLiteral: {
        tokens_.move();
        return std::make_shared<Ast::IntLiteral>(tk->span, static_cast<Tokens::RealTk*>(&*tk)->value);
    }
    case TokenType::Not: {
        tokens_.move();

    }
    case TokenType::PLUS:
    case TokenType::MINUS: {
        auto&& lit = tokens_.moveGet();
        if (lit) {
            Tokens::Span span{tk->span.line, tk->span.start, lit->span.end};
            tokens_.move();

            if (lit->type == TokenType::IntLiteral) {
                return std::make_shared<Ast::IntLiteral>(
                    span,
                    static_cast<Tokens::IntTk*>(&*tk)->value * (tk->type == TokenType::MINUS ? -1 : 1)
                );
            }
            if (lit->type == TokenType::RealLiteral) {
                return std::make_shared<Ast::RealLiteral>(
                    span,
                    static_cast<Tokens::RealTk*>(&*tk)->value * (tk->type == TokenType::MINUS ? -1 : 1)
                );
            }
        }
        
        // err
        return nullptr;
    }
    case TokenType::Identifier: {
        tokens_.move();
        auto&& modif = parseModifiablePrimary();
        if (modif) {
            auto&& bracket = tokens_.get();
            if (bracket && bracket->type == TokenType::BRACKET_OPEN) {
                // Routine call

                auto&& res = std::make_shared<Ast::RoutineCall>(tk->span, std::move(modif));

                std::shared_ptr<Tokens::BaseTk> nextArgTk;
                while ((nextArgTk = tokens_.get()) != nullptr) {
                    tk = std::move(nextArgTk);
                    if (tk->type == TokenType::BRACKET_CLOSE) {
                        break;
                    } else {
                        auto&& arg = parseExpr();
                        while ((nextArgTk = tokens_.get()) != nullptr) {
                            tk = std::move(nextArgTk);
                            if (tk->type == TokenType::BRACKET_CLOSE)
                                break;

                            tokens_.move();
                            if (tk->type == TokenType::COMMA)
                                break;
                        }

                        res->args.emplace_back(std::move(arg));

                        if (tk->type == TokenType::BRACKET_CLOSE)
                            break;
                    }
                }

                if (tk->type != TokenType::BRACKET_CLOSE) {
                    // err
                    return nullptr;
                }

                res->span.end = tk->span.end;
                return res;
            }

            return modif;
        }
    }
    }

    return nullptr;
}

std::shared_ptr<Ast::Expr> Parser::parseModifiablePrimary() {
    auto&& id = tokens_.get();
    if (!id || id->type != TokenType::Identifier) {
        // err
        return nullptr;
    }

    auto&& res = std::make_shared<Ast::IdRef>(id->span, ID_STR(id));
    std::shared_ptr<Ast::CompoundPrimary> head = res;

    std::shared_ptr<Tokens::BaseTk> tk;
    while (true) {
        auto&& next = tokens_.moveGet();
        if (!next)
            break;
        
        if (next->type == TokenType::DOT) {
            tk = std::move(next);

            auto&& nextId = tokens_.moveGet();
            if (!nextId || nextId->type != TokenType::Identifier) {
                // err
                break;
            }

            auto&& node = std::make_shared<Ast::IdRef>(nextId->span, ID_STR(nextId));
            head->next = node;
            head = std::move(node);
        } else if (next->type == TokenType::SQUARE_BRACKET_OPEN) {
            tk = std::move(next);
            tokens_.move();
            
            auto&& expr = parseExpr();
            auto&& node = std::make_shared<Ast::ArrayAccess>(std::move(expr));
            head->next = node;
            head = std::move(node);
        } else
            break;
    }

    return res;
}

/*******************************************************************************************************************/
/*************************************************** Statements ****************************************************/
/*******************************************************************************************************************/

std::shared_ptr<Ast::PrintStmt> Parser::parsePrintStmt() {
    std::cout << "= Parsing print\n";

    auto&& tk = tokens_.get();
    auto&& res = std::make_shared<Ast::PrintStmt>(tk->span);
    std::shared_ptr<Ast::Expr> lastExpr{nullptr};

    do {
        tk = tokens_.moveGet();
        tokens_.move();
        if (!tk || tk->type == TokenType::ENDLINE || tk->type == TokenType::SEMICOLON)
            break;

        auto&& nextExpr = parseExpr();
        if (!nextExpr) {
            // err
            break;
        }
        lastExpr = std::move(nextExpr);
        res->args.emplace_back(lastExpr);

        auto&& comma = tokens_.get();
        if (!comma || comma->type != TokenType::COMMA) {
            // err
            break;
        }
    } while (true);

    if (lastExpr)
        res->span.end = lastExpr->span.end;
    return res;
}

std::shared_ptr<Ast::IfStmt> Parser::parseIfStmt() {
    std::cout << "= Parsing if\n";

    auto&& tk = tokens_.get();
    auto&& res = std::make_shared<Ast::IfStmt>(tk->span);
    
    tokens_.move();
    auto&& expr = parseExpr();
    if (!expr) {
        // err
        return nullptr;
    }

    tk = tokens_.get();
    if (!tk || tk->type != TokenType::Then) {
        // err
        return nullptr;
    }
    
    tokens_.move();
    auto&& body = parseBlock();
    res->body = std::move(body);
    
    tk = tokens_.get();
    tokens_.move();
    if (tk && tk->type == TokenType::Else) {
        auto&& body = parseBlock();
        res->elseBody = std::move(body);
    }
    tk = tokens_.get();
    if (!tk || tk->type != TokenType::End) {
        // err
        return nullptr;
    }
    
    tokens_.move();
    res->span.end = tk->span.end;
    return res;
}

std::shared_ptr<Ast::ForStmt> Parser::parseForStmt() {
    std::cout << "= Parsing for\n";

    auto&& tk = tokens_.get();
    auto&& res = std::make_shared<Ast::ForStmt>(tk->span);
    
    tk = tokens_.moveGet();
    if (!tk || tk->type != TokenType::Identifier) {
        // err
        return nullptr;
    }
    res->counterId = ID_STR(tk);

    tk = tokens_.moveGet();
    if (!tk || tk->type != TokenType::In) {
        // err
        return nullptr;
    }

    tokens_.move();
    auto&& range = parseRangeSpecifier();
    if (!range) {
        // err
        return nullptr;
    }
    
    tk = tokens_.moveGet();
    if (!tk) {
        // err
        // expected loop
        return nullptr;
    }
    
    if (tk->type == TokenType::Reverse) {
        res->reverse = true;
        tk = tokens_.moveGet();
        if (!tk) {
            // err
            // same as above, expected loop
            return nullptr;
        }
    }

    if (tk->type != TokenType::Loop) {
        // err
        // same as above, expected loop
        return nullptr;
    }

    tokens_.move();
    auto&& body = parseBlock();
    res->body = std::move(body);

    tk = tokens_.get();
    if (!tk || tk->type != TokenType::End) {
        // err
        return nullptr;
    }
    
    tokens_.move();
    res->span.end = tk->span.end;
    return res;
}

std::shared_ptr<Ast::RangeSpecifier> Parser::parseRangeSpecifier() {
    std::cout << "= Parsing range\n";

    auto&& tk = tokens_.get();
    if (!tk)
        return nullptr;

    if (tk->type == TokenType::Identifier) {
        tokens_.move();
        return std::make_shared<Ast::RangeSpecifier>(tk->span);
    }

    tokens_.move();
    auto&& start = parseExpr();
    if (!start) {
        // err
        return nullptr;
    }

    auto&& range = tokens_.get();
    if (!range || range->type != TokenType::DOUBLE_DOT) {
        // err
        return nullptr;
    }

    auto&& end = parseExpr();
    if (!end) {
        // expr
        return nullptr;
    }

    return std::make_shared<Ast::IntRange>(
        Tokens::Span{tk->span.line, tk->span.start, end->span.end},
        std::move(start),
        std::move(end)
    );
}