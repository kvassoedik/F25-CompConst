#include "ast/Parser.h"
#include <iostream>
#include <sstream>

#define ID_STR(tk) static_cast<Tokens::IdentifierTk*>(&*tk)->identifier
#define DEBUG_HINT(str) std::cout \
    << "= Parsing " << ANSI_START ANSI_BOLD ANSI_APPLY << str << ANSI_RESET" " \
    << file_->fileName() << ":" << tk->span.line << ":" << tk->span.start - file_->lineStarts[tk->span.line-1] + 1 << "\n"

int Parser::configure(int* argc, char** argv) {
    return 0;
}

void Parser::feed(TokenList tokens) { tokens_ = std::move(tokens); }

void Parser::parse() {
    while (nextNode(currBlock_));
}

bool Parser::hasErrors() const {
    return reporter_.hasErrors();
}

/*******************************************************************************************************************/

bool Parser::nextNode(std::shared_ptr<Ast::Block>& block) {
    auto&& tk = tokens_.get();
    if (!tk) return false;
    
    switch (tk->type) {
    case TokenType::Var: {
        auto&& node = parseVarDecl();
        if (node) {
            block->declarations.emplace_back(node);
            block->declMap.emplace(node->id, std::move(node));
        }
        break;
    }
    case TokenType::Routine: {
        auto&& node = parseRoutine();
        if (node) {
            block->declarations.emplace_back(node);
            block->declMap.emplace(node->id, std::move(node));
        }
        break;
    }
    case TokenType::Type: {
        auto&& node = parseTypeDecl();
        if (node) {
            block->typeMap.emplace(node->id, node->type);
        }
        break;
    }
    case TokenType::Print: {
        auto&& node = parsePrintStmt();
        if (node) {
            block->statements.emplace_back(std::move(node));
        }
        break;
    }
    case TokenType::If: {
        auto&& node = parseIfStmt();
        if (node) {
            block->statements.emplace_back(std::move(node));
        }
        break;
    }
    case TokenType::For: {
        auto&& node = parseForStmt();
        if (node) {
            block->statements.emplace_back(std::move(node));
        }
        break;
    }
    case TokenType::While: {
        auto&& node = parseWhileStmt();
        if (node) {
            block->statements.emplace_back(std::move(node));
        }
        break;
    }
    case TokenType::Identifier: {
        auto&& node = parseModifiablePrimary();
        if (node) {
            tk = tokens_.get();
            if (!tk || tk->type != TokenType::ASSIGNMENT) {
                // err, rogue statement
                break;
            }

            auto&& res = std::make_shared<Ast::Assignment>(node->span);
            res->left = std::move(node);
            auto&& expr = parseExpr();
            if (expr) {
                res->span.end = expr->span.end;
                res->val = std::move(expr);
                block->statements.emplace_back(std::move(res));
            } else {
                // err
            }

        }
        break;
    }
    case TokenType::ENDLINE:
    case TokenType::SEMICOLON: {
        tokens_.move();
        break;
    }
    default: {
        tokens_.move();
        saveError("invalid statement", tk->span);
    }
    }

    return true;
}

void Parser::saveError(std::string reason, Tokens::Span span) {
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
    auto&& res = Ast::mk<Ast::Block>(Tokens::Span{0, 0, 0});
    res->parent = currBlock_;
    currBlock_ = res;

    while (true) {
        auto&& tk = tokens_.get();
        if (!tk || tk->type == TokenType::End || tk->type == TokenType::Else)
            break;
        if (!nextNode(res))
            break;
    }

    currBlock_ = currBlock_->parent;
    return res;
}

std::shared_ptr<Ast::Routine> Parser::parseRoutine() {
    auto&& tk = tokens_.get();
    DEBUG_HINT("routine");

    // skipping over 'routine' keyword immediately
    auto&& id = tokens_.moveGet();
    if (!id || id->type != TokenType::Identifier) {
        saveError("expected routine identifier", id->span);
        return nullptr;
    }
    tokens_.move();
    auto&& res = Ast::mk<Ast::Routine>(tk->span, ID_STR(id));

    {
        // Previously defined?
        auto&& prevDef = currBlock_->declMap.find(ID_STR(id));
        if (prevDef != currBlock_->declMap.end()) {
            saveError("redefinition of identifier " + ID_STR(id), id->span);
            reporter_.report({
                .level = CompileMsg::Level::Appendix,
                .message = "previously defined here:",
                .span = prevDef->second->span,
            });
            return nullptr;
        }
    }

    tk = tokens_.get();
    if (!tk || tk->type != TokenType::BRACKET_OPEN) {
        saveError(
            "expected '(' after routine identifier",
            {tk->span.line, tk->span.start, tk->span.start+1}
        );
        return nullptr;
    }
    tokens_.move();

    // Parse params
    {
        std::shared_ptr<Tokens::BaseTk> nextParamTk;
        while ((nextParamTk = tokens_.get()) != nullptr) {
            tk = std::move(nextParamTk);
            if (tk->type == TokenType::BRACKET_CLOSE) {
                break;
            } else {
                auto&& param = parseRoutineParam();
                if (param)
                    res->params.emplace_back(std::move(param));
                
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
        saveError(
            "expected ')' after routine parameters",
            {tk->span.line, tk->span.end, tk->span.end + 1}
        );
        return nullptr;
    }
    res->span.end = bracket->span.end;

    tk = tokens_.moveGet();
    if (tk && tk->type == TokenType::COLON) {
        tokens_.move();
        auto&& returnType = parseType();
        res->retType = returnType;
        res->span.end = tk->span.end;
    }

    if ((tk = tokens_.get()) != nullptr) {
        if (tk->type == TokenType::Is) {
            tokens_.move();
            res->body = parseBlock();

            tk = tokens_.get();
            if (!tk || tk->type != TokenType::End) {
                // err
                return nullptr;
            }
            tokens_.move();
        } else if (tk->type == TokenType::ROUTINE_ARROW) {
            tokens_.move();
            res->body = parseBlock();
        } else if (tk->type != TokenType::ENDLINE || tk->type != TokenType::SEMICOLON) {
            saveError(
                "expected 'is' before routine body",
                {tk->span.line, tk->span.start, tk->span.start + 1}
            );
            return nullptr;
        }
    }

    if (res->body)
        res->span.end = res->body->span.end;
    return res;
}

std::shared_ptr<Ast::Var> Parser::parseRoutineParam() {
    auto&& tk = tokens_.get();
    if (!tk || tk->type != TokenType::Identifier) {
        saveError(
            "expected parameter identifier",
            tk ? tk->span : Tokens::Span{file_->lineStarts.back(), file_->size(), file_->size()}
        );
        return nullptr;
    }

    auto&& res = Ast::mk<Ast::Var>(tk->span, ID_STR(tk));

    auto&& colon = tokens_.moveGet();
    if (!colon || colon->type != TokenType::COLON) {
        saveError(
            "expected ':' after parameter identifier",
            {colon ? colon->span.line : tk->span.line, colon ? colon->span.start : tk->span.end, colon ? colon->span.end : tk->span.end+1}
        );
        return res;
    }

    tokens_.move();
    auto&& type = parseType();
    res->type = std::move(type);

    return res;
}

std::shared_ptr<Ast::Type> Parser::parseType() {
    auto&& tk = tokens_.get();
    if (!tk)
        return Ast::mk<Ast::Type>(
            Tokens::Span{file_->lineStarts.back(), file_->size(), file_->size()},
            Ast::TypeEnum::ERROR
        );

    DEBUG_HINT("type");

    switch (tk->type)
    {
    case TokenType::IntegerType: {
        tokens_.move();
        return Ast::mk<Ast::Type>(tk->span, Ast::TypeEnum::Int);
    }
    case TokenType::RealType: {
        tokens_.move();
        return Ast::mk<Ast::Type>(tk->span, Ast::TypeEnum::Real);
    }
    case TokenType::BooleanType: {
        tokens_.move();
        return Ast::mk<Ast::Type>(tk->span, Ast::TypeEnum::Bool);
    }
    case TokenType::Identifier: {
        tokens_.move();
        return Ast::mk<Ast::TypeRef>(tk->span, ID_STR(tk));
    }
    case TokenType::Array: {
        auto&& res = Ast::mk<Ast::ArrayType>(tk->span);

        auto&& bracketOpen = tokens_.moveGet();
        if (!bracketOpen || bracketOpen->type != TokenType::SQUARE_BRACKET_OPEN) {
            saveError(
                "expected '[' when declaring an array type",
                {tk->span.line, tk->span.start, tk->span.end+1}
            );

            return Ast::mk<Ast::Type>(
                Tokens::Span{tk->span.line, tk->span.start, bracketOpen ? bracketOpen->span.end : tk->span.end},
                Ast::TypeEnum::ERROR
            );
        }

        tokens_.move();
        res->size = parseExpr();

        auto&& bracketClose = tokens_.get();
        if (!bracketClose || bracketClose->type != TokenType::SQUARE_BRACKET_CLOSE) {
            unsigned long start = bracketClose ? bracketClose->span.start : bracketOpen->span.start+1;
            saveError(
                "expected ']' after expression when declaring an array type",
                {tk->span.line, start, start+1}
            );
            
            return Ast::mk<Ast::Type>(
                Tokens::Span{tk->span.line, tk->span.start, bracketClose ? bracketClose->span.end : bracketOpen->span.end},
                Ast::TypeEnum::ERROR
            );
        }

        tokens_.move();
        res->elemType = parseType();
        res->span.end = res->elemType->span.end;

        if (res->elemType->code == Ast::TypeEnum::ERROR) {
            saveError(
                "expected type of array elements",
                {bracketClose->span.line, bracketClose->span.end, bracketClose->span.end+1}
            );
        }

        return res;
    }
    case TokenType::Record: {
        auto&& res = Ast::mk<Ast::RecordType>(tk->span);

        auto&& contents = tokens_.moveGet();
        while (contents) {
            if (contents->type == TokenType::End) {
                res->span.end = contents->span.end;
                return res;
            }

            auto&& var = parseVarDecl();
            if (var)
                res->members.emplace_back(std::move(var));

            auto&& nextTk = tokens_.moveGet();
            if (!nextTk)
                break;
            contents = std::move(nextTk);
        }

        res->code = Ast::TypeEnum::ERROR;
        res->span.end = contents->span.end;
        return res;
    }
    }

    return Ast::mk<Ast::Type>(tk->span, Ast::TypeEnum::ERROR);
}

std::shared_ptr<Ast::Var> Parser::parseVarDecl() {
    auto&& tk = tokens_.get();
    if (!tk)
        return nullptr;

    if (tk->type != TokenType::Var) {
        saveError("Expected 'var' before new declaration", tk->span);
        return nullptr;
    }

    auto&& id = tokens_.moveGet();
    if (!id || id->type != TokenType::Identifier) {
        // err
        return nullptr;
    }

    Tokens::Span headerSpan{tk->span.line, tk->span.start, id->span.end};
    std::shared_ptr<Ast::Type> explicitType{nullptr};

    auto&& colon = tokens_.moveGet();
    if (colon && colon->type == TokenType::COLON) {
        tokens_.move();
        explicitType = parseType();
        headerSpan.end = explicitType->span.end;
    }

    auto&& res = std::make_shared<Ast::Var>(headerSpan, ID_STR(id));

    auto&& is = tokens_.get();
    if (!is || is->type != TokenType::Is) {
        if (!explicitType) {
            // err
            return nullptr;
        }

        res->type = explicitType;
        return res;
    }

    tokens_.move();
    auto&& expr = parseExpr();
    res->val = expr;
    res->type = explicitType
        ? explicitType
        : (expr ? expr->type : std::make_shared<Ast::Type>(headerSpan, Ast::TypeEnum::ERROR));

    return res;
}

std::shared_ptr<Ast::TypeDecl> Parser::parseTypeDecl() {
    return nullptr;
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

        Ast::ExprEnum code;
        switch (tk->type)
        {
        case TokenType::And: {code = Ast::ExprEnum::And; break;}
        case TokenType::Or: {code = Ast::ExprEnum::Or; break;}
        case TokenType::Xor: {code = Ast::ExprEnum::Xor; break;}
        }
        auto&& expr = std::make_shared<Ast::ExprOperation>(Tokens::Span{left->span.line, left->span.start, right->span.end}, code);
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

        Ast::ExprEnum code;
        switch (tk->type)
        {
        case TokenType::LESS_THAN: {code = Ast::ExprEnum::LESS_THAN; break;}
        case TokenType::LESS_OR_EQUAL: {code = Ast::ExprEnum::LESS_OR_EQUAL; break;}
        case TokenType::MORE_THAN: {code = Ast::ExprEnum::MORE_THAN; break;}
        case TokenType::MORE_OR_EQUAL: {code = Ast::ExprEnum::MORE_OR_EQUAL; break;}
        case TokenType::EQUAL: {code = Ast::ExprEnum::EQUAL; break;}
        case TokenType::UNEQUAL: {code = Ast::ExprEnum::UNEQUAL; break;}
        }
        auto&& relation = std::make_shared<Ast::ExprOperation>(Tokens::Span{left->span.line, left->span.start, right->span.end}, code);
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

        Ast::ExprEnum code;
        switch (tk->type)
        {
        case TokenType::TIMES: {code = Ast::ExprEnum::Multiply; break;}
        case TokenType::DIVIDE: {code = Ast::ExprEnum::Divide; break;}
        case TokenType::MODULO: {code = Ast::ExprEnum::Modulo; break;}
        }
        auto&& simple = std::make_shared<Ast::ExprOperation>(Tokens::Span{left->span.line, left->span.start, right->span.end}, code);
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

        Ast::ExprEnum code;
        switch (tk->type)
        {
        case TokenType::PLUS: {code = Ast::ExprEnum::Add; break;}
        case TokenType::MINUS: {code = Ast::ExprEnum::Subtract; break;}
        }
        auto&& factor = std::make_shared<Ast::ExprOperation>(Tokens::Span{left->span.line, left->span.start, right->span.end}, code);
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

std::shared_ptr<Ast::ModifiablePrimary> Parser::parseModifiablePrimary() {
    auto&& id = tokens_.get();
    if (!id || id->type != TokenType::Identifier) {
        // err
        return nullptr;
    }

    auto&& res = std::make_shared<Ast::IdRef>(id->span, ID_STR(id));
    std::shared_ptr<Ast::ModifiablePrimary> head = res;

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
            auto&& node = std::make_shared<Ast::ArrayAccess>(Tokens::Span{tk->span.line, tk->span.start, expr->span.end}, std::move(expr));
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
    auto&& tk = tokens_.get();
    DEBUG_HINT("print");

    auto&& res = std::make_shared<Ast::PrintStmt>(tk->span);
    std::shared_ptr<Ast::Expr> lastExpr{nullptr};

    tk = tokens_.moveGet();
    if (!tk || tk->type == TokenType::ENDLINE || tk->type == TokenType::SEMICOLON)
        return res;

    do {
        auto&& nextExpr = parseExpr();
        if (!nextExpr) {
            // err
            break;
        }
        lastExpr = std::move(nextExpr);
        res->args.emplace_back(lastExpr);

        tk = tokens_.get();
        if (!tk || tk->type == TokenType::ENDLINE || tk->type == TokenType::SEMICOLON)
            break;

        if (tk->type != TokenType::COMMA) {
            // err
            break;
        }
        tokens_.move();
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

std::shared_ptr<Ast::WhileStmt> Parser::parseWhileStmt() {
    std::cout << "= Parsing while\n";

    auto&& tk = tokens_.get();
    auto&& res = std::make_shared<Ast::WhileStmt>(tk->span);
    tokens_.move();

    auto&& expr = parseExpr();
    if (!expr) {
        // err
        return nullptr;
    }

    tk = tokens_.get();
    if (!tk || tk->type != TokenType::Loop) {
        // err
        return nullptr;
    }

    tokens_.move();
    auto&& body = parseBlock();
    if (!body) {
        // err
        return nullptr;
    }

    tk = tokens_.get();
    if (!tk || tk->type != TokenType::End) {
        // err
        return nullptr;
    }

    res->span.end = tk->span.end;
    res->condition = std::move(expr);
    res->body = std::move(body);
    return res;
}

std::shared_ptr<Ast::ReturnStmt> Parser::parseReturnStmt() {
    auto&& tk = tokens_.get();
    auto&& res = std::make_shared<Ast::ReturnStmt>(tk->span);
    tokens_.move();

    auto&& expr = parseExpr();
    if (!expr) {
        // err
        return nullptr;
    }

    res->val = std::move(expr);
    return res;
}