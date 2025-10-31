#include "parser/Parser.h"
#include <iostream>
#include <sstream>

#define ID_STR(tk) static_cast<Tokens::IdentifierTk*>(&*tk)->identifier

int Parser::configure(int* argc, char** argv) {
    return 0;
}

void Parser::run() {
    while (nextNode());
}

/*******************************************************************************************************************/

bool Parser::nextNode() {
    auto&& [success, tk] = parseEntity();
    if (!success) {
        if (!tk)
            return false;

        tokens_.move();
        saveError("invalid statement (next node)", tk->span);
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

std::pair<bool, std::shared_ptr<Tokens::BaseTk>> Parser::parseEntity() {
    auto&& tk = tokens_.get();
    if (!tk)
        return {false, nullptr};
    
    switch (tk->type) {
    case TokenType::Var: {
        auto&& node = parseVarDecl();
        if (node)
            currBlock_->units.push_back(std::move(node));
        break;
    }
    case TokenType::Routine: {
        auto&& node = parseRoutine();
        if (node)
            currBlock_->units.push_back(std::move(node));
        break;
    }
    case TokenType::Type: {
        auto&& node = parseTypeDecl();
        if (node)
            currBlock_->units.push_back(std::move(node));
        break;
    }
    case TokenType::Print: {
        auto&& node = parsePrintStmt();
        if (node)
            currBlock_->units.push_back(std::move(node));
        break;
    }
    case TokenType::If: {
        auto&& node = parseIfStmt();
        if (node)
            currBlock_->units.push_back(std::move(node));
        break;
    }
    case TokenType::For: {
        auto&& node = parseForStmt();
        if (node)
            currBlock_->units.push_back(std::move(node));
        break;
    }
    case TokenType::While: {
        auto&& node = parseWhileStmt();
        if (node)
            currBlock_->units.push_back(std::move(node));
        break;
    }
    case TokenType::Return: {
        auto&& node = parseReturnStmt();
        if (node)
            currBlock_->units.push_back(std::move(node));
        break;
    }
    case TokenType::Identifier: {
        auto&& node = parseModifiablePrimaryOrRoutineCall();
        if (node) {
            tk = tokens_.get();
            if (!tk || tk->type != TokenType::ASSIGNMENT) {
                if (tk) {
                    saveError("invalid statement", tk->span);
                    tokens_.move();
                } else {
                    saveError("invalid statement", file_->eof());
                }
                break;
            }

            tokens_.move();

            // Cannot assign to a return value of routine call 
            if (node->code == Ast::ExprEnum::RoutineCall) {
                saveError("illegal assignment to routine return value", node->span);
                break;
            }

            auto&& res = Ast::mk<Ast::Assignment>(node->span);
            res->left = std::move(reinterpret_cast<std::shared_ptr<Ast::ModifiablePrimary>&>(node));
            auto&& expr = parseExpr();
            if (expr) {
                res->span.end = expr->span.end;
                res->val = std::move(expr);
                currBlock_->units.push_back(std::move(res));
            } else {
                auto afterAssign = tokens_.get();
                saveError("expected expression after '='",
                            afterAssign ? afterAssign->span : res->span);
            }

        }
        break;
    }
    case TokenType::ENDLINE:
    case TokenType::SEMICOLON: {
        tokens_.move();
        break;
    }
    default:
        return {false, std::move(tk)};
    }

    return {true, std::move(tk)};
}

void Parser::finalizeCurrBlock() {
    if (!currBlock_->units.empty()) {
        currBlock_->span.line = currBlock_->units.front()->span.line;
        currBlock_->span.start = currBlock_->units.front()->span.start;
        currBlock_->span.end = currBlock_->units.back()->span.end;
    }
}

std::shared_ptr<Ast::Block> Parser::parseRoutineBody(Tokens::Span initSpan) {
    auto&& res = Ast::mk<Ast::Block>(initSpan);
    res->parent = currBlock_;
    currBlock_ = res;

    while (true) {
        auto&& [success, tk] = parseEntity();
        if (!success) {
            if (!tk || tk->type == TokenType::End)
                break;

            tokens_.move();
            saveError("invalid statement (in routine body)", tk->span);
        }
    }

    finalizeCurrBlock();
    currBlock_ = currBlock_->parent;
    return res;
}

void Parser::parseIfBody(std::shared_ptr<Ast::IfStmt>& parent, Tokens::Span initSpan) {
    {
        auto&& res = Ast::mk<Ast::Block>(initSpan);
        res->parent = currBlock_;
        currBlock_ = res;
    }

    bool elseBodyFound = false;
    while (true) {
        auto&& [success, tk] = parseEntity();
        if (!success) {
            if (!tk || tk->type == TokenType::End)
                break;

            tokens_.move();
            if (tk->type == TokenType::Else) {
                if (elseBodyFound) {
                    saveError("multiple 'else' keywords encountered", tk->span);
                    continue;
                }

                elseBodyFound = true;
                finalizeCurrBlock();
                auto&& elseBody = Ast::mk<Ast::Block>(Tokens::Span{tk->span.line, tk->span.end+1, tk->span.end+1});
                elseBody->parent = currBlock_->parent;
                parent->body = std::move(currBlock_);
                currBlock_ = std::move(elseBody);
                continue;
            }

            saveError("invalid statement (in if body)", tk->span);
        }
    }

    auto&& tk = tokens_.get();
    if (!tk || tk->type != TokenType::End) {
        saveError(
            "expected 'end' after to close 'if' statement",
            tk ? tk->span : file_->eof()
        );
        return;
    }
    tokens_.move();

    finalizeCurrBlock();
    if (elseBodyFound)
        parent->elseBody = currBlock_;
    else
        parent->body = currBlock_;
    currBlock_ = currBlock_->parent;
}

std::shared_ptr<Ast::Routine> Parser::parseRoutine() {
    auto&& tk = tokens_.get();

    // skipping over 'routine' keyword immediately
    auto&& id = tokens_.moveGet();
    if (!id || id->type != TokenType::Identifier) {
        saveError("expected routine identifier", id->span);
        return nullptr;
    }

    auto&& res = Ast::mk<Ast::Routine>(tk->span, ID_STR(id));

    tk = tokens_.moveGet();
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
                    res->params.push_back(std::move(param));
                
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
            res->body = parseRoutineBody(Tokens::Span{tk->span.line, tk->span.end+1, tk->span.end+1});

            tk = tokens_.get();
            if (!tk || tk->type != TokenType::End) {
                saveError("expected 'end' to close routine body",
                              tk ? tk->span : res->span);
                return nullptr;
            }
            tokens_.move();
        } else if (tk->type == TokenType::ROUTINE_ARROW) {
            tokens_.move();

            auto&& body = Ast::mk<Ast::Block>(Tokens::Span{tk->span.line, tk->span.end+1, tk->span.end+1});
            auto&& retStmt = Ast::mk<Ast::ReturnStmt>(body->span);
            retStmt->val = std::move(parseExpr());
            body->units.push_back(std::move(retStmt));
            res->body = std::move(body);
        } else if (tk->type != TokenType::ENDLINE && tk->type != TokenType::SEMICOLON) {
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
                tokens_.move();
                res->span.end = contents->span.end;
                return res;
            } else if (
                contents->type == TokenType::SEMICOLON
                || contents->type == TokenType::ENDLINE
            ) {
                contents = std::move(tokens_.moveGet());
                continue;
            }

            auto&& var = parseVarDecl();
            if (var)
                res->members.push_back(std::move(var));

            auto&& nextTk = tokens_.get();
            if (!nextTk)
                break;
            contents = std::move(nextTk);
        }

        res->code = Ast::TypeEnum::ERROR;
        res->span.end = contents ? contents->span.end : file_->eof().end;
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
        saveError(
            "expected identifier after 'var'",
            id ? id->span : file_->eof()
        );
        return nullptr;
    }

    Tokens::Span headerSpan{tk->span.line, tk->span.start, id->span.end};
    auto&& res = Ast::mk<Ast::Var>(headerSpan, ID_STR(id));
    std::shared_ptr<Ast::Type> explicitType{nullptr};

    auto&& colon = tokens_.moveGet();
    if (colon && colon->type == TokenType::COLON) {
        tokens_.move();
        explicitType = parseType();
        headerSpan.end = explicitType->span.end;
    }

    auto&& is = tokens_.get();
    if (!is || is->type != TokenType::Is) {
        if (!explicitType) {
            saveError("expected ': type' in variable declaration",
                          is ? is->span : headerSpan);
            return nullptr;
        }

        res->type = explicitType;
        return res;
    }

    tokens_.move();
    auto&& expr = parseExpr();
    res->val = std::move(expr);
    res->type = std::move(explicitType);

    return res;
}

std::shared_ptr<Ast::TypeDecl> Parser::parseTypeDecl() {
    auto&& tk = tokens_.get();
    auto&& id = tokens_.moveGet();
    if (!id || id->type != TokenType::Identifier) {
        saveError(
            "expected type identifier",
            id ? id->span : file_->eof()
        );
        return nullptr;
    }

    auto&& res = Ast::mk<Ast::TypeDecl>(tk->span, ID_STR(id));
    auto&& is = tokens_.moveGet();
    if (!is || is->type != TokenType::Is) {
        saveError(
            "expected 'is' after type identifier",
            is ? is->span : file_->eof()
        );
        return nullptr;
    }

    tokens_.move();
    auto&& type = parseType();
    res->type = std::move(type);
    res->span.end = res->type->span.end;
    return res;
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
            Tokens::Span caret{
                tk->span.line,
                tk->span.end + 1,
                tk->span.end + 4
            };
            saveError("expected expression after logical operator", caret);
            break;
        }

        Ast::ExprEnum code;
        switch (tk->type)
        {
        case TokenType::And: {code = Ast::ExprEnum::And; break;}
        case TokenType::Or: {code = Ast::ExprEnum::Or; break;}
        case TokenType::Xor: {code = Ast::ExprEnum::Xor; break;}
        }
        auto&& expr = Ast::mk<Ast::BinaryExpr>(Tokens::Span{left->span.line, left->span.start, right->span.end}, code);
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
            Tokens::Span caret{
                tk->span.line,
                tk->span.end + 1,
                tk->span.end + 4
            };
            saveError("expected expression after a comparison operator", caret);
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
        auto&& relation = Ast::mk<Ast::BinaryExpr>(Tokens::Span{left->span.line, left->span.start, right->span.end}, code);
        relation->left = std::move(left);
        relation->right = std::move(right);
        left = std::move(relation);
    }
    
    return left;
}

std::shared_ptr<Ast::Expr> Parser::parseSimple() {
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
            Tokens::Span caret{
                tk->span.line,
                tk->span.end + 1,
                tk->span.end + 4
            };
            saveError("expected expression after a summand operator", caret);
            break;
        }

        Ast::ExprEnum code;
        switch (tk->type)
        {
        case TokenType::PLUS: {code = Ast::ExprEnum::Add; break;}
        case TokenType::MINUS: {code = Ast::ExprEnum::Subtract; break;}
        }
        auto&& simple = Ast::mk<Ast::BinaryExpr>(Tokens::Span{left->span.line, left->span.start, right->span.end}, code);
        simple->left = std::move(left);
        simple->right = std::move(right);
        left = std::move(simple);
    }
    
    return left;
}

std::shared_ptr<Ast::Expr> Parser::parseSummand() {
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
            Tokens::Span caret{
                tk->span.line,
                tk->span.end + 1,
                tk->span.end + 4
            };
            saveError("expected expression after a factor operator", caret);
            break;
        }

        Ast::ExprEnum code;
        switch (tk->type)
        {
        case TokenType::TIMES: {code = Ast::ExprEnum::Multiply; break;}
        case TokenType::DIVIDE: {code = Ast::ExprEnum::Divide; break;}
        case TokenType::MODULO: {code = Ast::ExprEnum::Modulo; break;}
        }
        auto&& factor = Ast::mk<Ast::BinaryExpr>(Tokens::Span{left->span.line, left->span.start, right->span.end}, code);
        factor->left = std::move(left);
        factor->right = std::move(right);
        left = std::move(factor);
    }
    
    return left;
}

std::shared_ptr<Ast::Expr> Parser::parseFactor() {
    auto&& tk = tokens_.get();
    if (!tk)
        return nullptr;

    if (tk->type == TokenType::BRACKET_OPEN) {
        tokens_.move();
        auto&& expr = parseExpr();
        if (!expr) {
            Tokens::Span caret{
                tk->span.line,
                tk->span.end,
                tk->span.end + 1
            };
            saveError("expected expression inside parentheses", caret);
            return nullptr;
        }

        auto&& bracket = tokens_.get();
        if (!bracket || bracket->type != TokenType::BRACKET_CLOSE) {
            Tokens::Span insideExpr{
                expr->span.line,
                expr->span.start,
                expr->span.end
            };
            saveError("expected ')' to close parenthesized expression", insideExpr);
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
        auto res = Ast::mk<Ast::BoolLiteral>(tk->span, true);
        res->known = true;
        res->type = baseTypes_[0];
        return res;
    }
    case TokenType::False: {
        tokens_.move();
        auto res = Ast::mk<Ast::BoolLiteral>(tk->span, false);
        res->known = true;
        res->type = baseTypes_[0];
        return res;
    }
    case TokenType::IntLiteral: {
        tokens_.move();
        auto res = Ast::mk<Ast::IntLiteral>(tk->span, static_cast<Tokens::IntTk*>(&*tk)->value);
        res->known = true;
        res->type = baseTypes_[1];
        return res;
    }
    case TokenType::RealLiteral: {
        tokens_.move();
        auto res = Ast::mk<Ast::RealLiteral>(tk->span, static_cast<Tokens::RealTk*>(&*tk)->value);
        res->known = true;
        res->type = baseTypes_[2];
        return res;
    }
    case TokenType::Not: {
        tokens_.move();
        auto&& res = Ast::mk<Ast::UnaryExpr>(tk->span, Ast::ExprEnum::Not);
        auto&& expr = parseExpr();
        res->val = std::move(expr);
        if (res->val)
            res->span.end = res->span.end;
        return res;
    }
    case TokenType::PLUS:
    case TokenType::MINUS: {
        auto&& opTk = tokens_.moveGet();
        if (opTk) {
            if (opTk->type == TokenType::IntLiteral) {
                tokens_.move();
                auto res = Ast::mk<Ast::IntLiteral>(
                    Tokens::Span{tk->span.line, tk->span.start, opTk->span.end},
                    static_cast<Tokens::IntTk*>(&*opTk)->value * (tk->type == TokenType::MINUS ? -1 : 1)
                );
                res->known = true;
                res->type = baseTypes_[1];
                return res;
            }
            if (opTk->type == TokenType::RealLiteral) {
                tokens_.move();
                auto res = Ast::mk<Ast::RealLiteral>(
                    Tokens::Span{tk->span.line, tk->span.start, opTk->span.end},
                    static_cast<Tokens::RealTk*>(&*opTk)->value * (tk->type == TokenType::MINUS ? -1 : 1)
                );
                res->known = true;
                res->type = baseTypes_[2];
                return res;
            }
            if (opTk->type == TokenType::Identifier) {
                auto&& operand = parsePrimary();
                if (tk->type == TokenType::MINUS) {
                    auto&& res = Ast::mk<Ast::UnaryExpr>(
                        Tokens::Span{tk->span.line, tk->span.start, operand ? operand->span.end : opTk->span.end},
                        Ast::ExprEnum::Negate
                    );
                    res->val = std::move(operand);

                    return res;
                }

                return operand;
            }
        }
        
        saveError("expected literal or primary expression after unary operator",
                      opTk ? opTk->span : tk->span);
        return nullptr;
    }
    case TokenType::Identifier: {
        auto&& modif = parseModifiablePrimaryOrRoutineCall();
        return modif;
    }
    }

    return nullptr;
}

std::shared_ptr<Ast::Expr> Parser::parseModifiablePrimaryOrRoutineCall() {
    auto&& id = tokens_.get();
    if (!id || id->type != TokenType::Identifier) {
        saveError("expected identifier",
                id ? id->span : Tokens::Span{currBlock_->span.line, currBlock_->span.start, currBlock_->span.start+1});
        return nullptr;
    }

    auto&& tk = tokens_.moveGet();
    if (tk->type == TokenType::BRACKET_OPEN) {
        auto res = parseRoutineCall(reinterpret_cast<std::shared_ptr<Tokens::IdentifierTk>&>(id));
        return res;
    }

    auto&& res = Ast::mk<Ast::IdRef>(id->span, ID_STR(id));
    std::shared_ptr<Ast::ModifiablePrimary> head = res;

    while (true) {
        if (!(tk = tokens_.get()))
            break;
        
        if (tk->type == TokenType::DOT) {
            auto&& nextId = tokens_.moveGet();
            if (!nextId || nextId->type != TokenType::Identifier) {
                saveError("expected field identifier after '.'",
                            nextId ? nextId->span : Tokens::Span{tk->span.line, tk->span.end, tk->span.end+1});
                break;
            }

            tokens_.move();
            auto&& node = Ast::mk<Ast::IdRef>(nextId->span, ID_STR(nextId));
            head->next = node;
            head = std::move(node);
        } else if (tk->type == TokenType::SQUARE_BRACKET_OPEN) {
            tokens_.move();
            
            auto&& node = Ast::mk<Ast::ArrayAccess>(tk->span);
            auto&& expr = parseExpr();
            if (!expr)
                return nullptr;
            
            tk = tokens_.get();
            if (!tk || tk->type != TokenType::SQUARE_BRACKET_CLOSE) {
                saveError(
                    "expected ']'",
                    tk ? tk->span : file_->eof()
                );
                return nullptr;
            }

            tokens_.move();
            node->span.end = tk->span.end;
            node->val = std::move(expr);
            head->next = node;
            head = std::move(node);
        } else
            break;
    }

    return res;
}

std::shared_ptr<Ast::RoutineCall> Parser::parseRoutineCall(std::shared_ptr<Tokens::IdentifierTk>& id) {
    auto&& tk = tokens_.get();
    if (!tk || tk->type != TokenType::BRACKET_OPEN) {
        return nullptr;
    }

    auto&& res = Ast::mk<Ast::RoutineCall>(id->span, ID_STR(id));

    std::shared_ptr<Tokens::BaseTk> nextArgTk;
    while ((nextArgTk = tokens_.moveGet()) != nullptr) {
        tk = std::move(nextArgTk);
        if (tk->type == TokenType::BRACKET_CLOSE) {
            break;
        } else {
            auto&& arg = parseExpr();
            while ((nextArgTk = tokens_.get()) != nullptr) {
                tk = std::move(nextArgTk);
                if (tk->type == TokenType::BRACKET_CLOSE || tk->type == TokenType::COMMA)
                    break;
                tokens_.move();
            }

            res->args.push_back(std::move(arg));

            if (tk->type == TokenType::BRACKET_CLOSE)
                break;
        }
    }

    if (tk->type != TokenType::BRACKET_CLOSE) {
        saveError("expected ')' to close routine call arguments",
                tk ? tk->span : res->span);
        return nullptr;
    }

    tokens_.move();
    res->span.end = tk->span.end;
    return res;
}

/*******************************************************************************************************************/
/*************************************************** Statements ****************************************************/
/*******************************************************************************************************************/

std::shared_ptr<Ast::PrintStmt> Parser::parsePrintStmt() {
    auto&& tk = tokens_.get();

    auto&& res = Ast::mk<Ast::PrintStmt>(tk->span);
    std::shared_ptr<Ast::Expr> lastExpr{nullptr};

    tk = tokens_.moveGet();
    if (!tk || tk->type == TokenType::ENDLINE || tk->type == TokenType::SEMICOLON)
        return res;

    do {
        auto&& nextExpr = parseExpr();
        if (!nextExpr) {
            saveError("expected expression in print arguments",
                        tk ? tk->span : res->span);
            break;
        }
        lastExpr = std::move(nextExpr);
        res->args.emplace_back(lastExpr);

        tk = tokens_.get();
        if (!tk || tk->type == TokenType::ENDLINE || tk->type == TokenType::SEMICOLON)
            break;

        if (tk->type != TokenType::COMMA) {
            saveError("expected ',' between print arguments",
                        tk->span);
            break;
        }
        tokens_.move();
    } while (true);

    if (lastExpr)
        res->span.end = lastExpr->span.end;
    return res;
}

std::shared_ptr<Ast::IfStmt> Parser::parseIfStmt() {
    auto&& tk = tokens_.get();
    auto&& res = Ast::mk<Ast::IfStmt>(tk->span);
    
    tokens_.move();
    auto&& expr = parseExpr();
    if (!expr) {
        auto afterIf = tokens_.get();
        saveError("expected condition after 'if'",
                    afterIf ? afterIf->span : res->span);
        return nullptr;
    }
    res->condition = std::move(expr);

    tk = tokens_.get();
    if (!tk || tk->type != TokenType::Then) {
        saveError("expected 'then' after 'if' condition",
                    tk ? tk->span : expr->span);
        return nullptr;
    }

    tokens_.move();
    parseIfBody(res, Tokens::Span{tk->span.line, tk->span.end+1, tk->span.end+1});

    return res;
}

std::shared_ptr<Ast::ForStmt> Parser::parseForStmt() {
    auto&& tk = tokens_.get();
    auto&& res = Ast::mk<Ast::ForStmt>(tk->span);
    
    tk = tokens_.moveGet();
    if (!tk || tk->type != TokenType::Identifier) {
        saveError(
            "expected counter identifier after 'for'",
            tk ? tk->span : file_->eof()
        );
        return nullptr;
    }
    res->counterId = ID_STR(tk);

    tk = tokens_.moveGet();
    if (!tk || tk->type != TokenType::In) {
        saveError(
            "expected 'in' after counter identifier",
            tk ? tk->span : file_->eof()
        );
        return nullptr;
    }

    tokens_.move();
    auto&& range = parseRangeSpecifier();
    if (!range) {
        auto afterIn = tokens_.get();
        saveError("expected range specifier after 'in'",
                    afterIn ? afterIn->span : res->span);
        return nullptr;
    }
    res->range = std::move(range);
    
    tk = tokens_.get();
    if (!tk) {
        saveError("expected 'loop' to start 'for' body", res->span);
        return nullptr;
    }
    
    if (tk->type == TokenType::Reverse) {
        res->reverse = true;
        tk = tokens_.moveGet();
        if (!tk) {
            saveError(
                "expected 'loop' after 'reverse' in 'for' statement",
                res->span
            );
            return nullptr;
        }
    }

    if (tk->type != TokenType::Loop) {
        saveError("expected 'loop' to start 'for' body",
                    tk->span);
        return nullptr;
    }

    tokens_.move();
    auto&& body = parseRoutineBody(Tokens::Span{tk->span.line, tk->span.end+1, tk->span.end+1});
    res->body = std::move(body);

    tk = tokens_.get();
    if (!tk || tk->type != TokenType::End) {
        saveError("expected 'end' to close 'for' statement",
                    tk ? tk->span : res->span);
        return nullptr;
    }
    
    tokens_.move();
    return res;
}

std::shared_ptr<Ast::RangeSpecifier> Parser::parseRangeSpecifier() {
    auto&& tk = tokens_.get();
    if (!tk)
        return nullptr;

    if (tk->type == TokenType::Identifier) {
        tokens_.move();
        auto&& res = Ast::mk<Ast::ArrayId>(tk->span);
        res->id = ID_STR(tk);
        return res;
    }

    auto&& res = Ast::mk<Ast::IntRange>(tk->span);
    auto&& start = parseExpr();
    if (!start) {
        saveError("expected first expression in range specifier",tk->span);
        return nullptr;
    }

    auto&& range = tokens_.get();
    if (!range || range->type != TokenType::DOUBLE_DOT) {
        saveError("expected '..' in range specifier",
                    range ? range->span : Tokens::Span{tk->span.line, start->span.end, start->span.end+1});
        return nullptr;
    }

    tokens_.move();
    auto&& end = parseExpr();
    if (!end) {
        auto&& afterDots = tokens_.get();
        saveError(
            "expected second expression after '..'",
            afterDots ? afterDots->span : file_->eof()
        );
        return nullptr;
    }

    res->span.end = end->span.end;
    res->start = std::move(start);
    res->end = std::move(end);
    return res;
}

std::shared_ptr<Ast::WhileStmt> Parser::parseWhileStmt() {
    auto&& tk = tokens_.get();
    auto&& res = Ast::mk<Ast::WhileStmt>(tk->span);
    tokens_.move();

    auto&& expr = parseExpr();
    if (!expr) {
        auto afterWhile = tokens_.get();
        saveError("expected condition after 'while'",
                    afterWhile ? afterWhile->span : res->span);
        return nullptr;
    }

    tk = tokens_.get();
    if (!tk || tk->type != TokenType::Loop) {
        saveError(
            "expected 'loop' to start 'while' body",
            tk ? tk->span : res->span
        );
        return nullptr;
    }

    tokens_.move();
    auto&& body = parseRoutineBody(Tokens::Span{tk->span.line, tk->span.end+1, tk->span.end+1});
    if (!body) {
        saveError("invalid 'while' body", tk->span);
        return nullptr;
    }

    tk = tokens_.get();
    if (!tk || tk->type != TokenType::End) {
        saveError(
            "expected 'end' to close 'while' statement",
            tk ? tk->span : res->span
        );
        return nullptr;
    }

    res->span.end = tk->span.end;
    res->condition = std::move(expr);
    res->body = std::move(body);
    return res;
}

std::shared_ptr<Ast::ReturnStmt> Parser::parseReturnStmt() {
    auto&& tk = tokens_.get();
    auto&& res = Ast::mk<Ast::ReturnStmt>(tk->span);
    tokens_.move();

    auto&& expr = parseExpr();
    if (!expr) {
        auto afterReturn = tokens_.get();
        saveError(
            "expected expression after 'return'",
            afterReturn ? afterReturn->span : res->span
        );
        return nullptr;
    }

    res->val = std::move(expr);
    return res;
}