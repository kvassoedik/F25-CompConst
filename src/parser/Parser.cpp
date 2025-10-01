#include "ast/Parser.h"
#include <iostream>
#include <sstream>

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

bool Parser::nextNode() {
    auto&& tk = tokens_.get();
    if (!tk) return false;

    startTk_ = std::move(tk);
    tokens_.move();
    
    switch (startTk_->type) {
    case TokenType::Routine: {
        parseRoutine();
        break;
    }

    default:
        break;
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
    std::shared_ptr<Ast::Block>& block = currBlock_;
    while (block) {
        auto&& it = block->declMap.find(id);
        if (it != block->declMap.end())
            return it->second;
            
        block = block->parent;
    }
    return {nullptr};
}

std::shared_ptr<Ast::Type> Parser::findNamedType(const std::string& id) {
    std::shared_ptr<Ast::Block>& block = currBlock_;
    while (block) {
        auto&& it = block->typeMap.find(id);
        if (it != block->typeMap.end())
            return it->second;
            
        block = block->parent;
    }
    return {nullptr};
}

void Parser::parseBlock(std::shared_ptr<Ast::Block>& parent) {

}

void Parser::parseRoutine() {
    std::cout << "= Parsing routine\n";

    auto&& id = tokens_.get();
    if (!id || id->type != TokenType::Identifier) {
        std::stringstream ss;
        ss << "expected routine identifier, got ";
        saveError(id->span, ss.str());
        return;
    }
    tokens_.move();

    {
        // Previously defined?
        auto&& prevDef = currBlock_->declMap.find(static_cast<Tokens::IdentifierTk*>(&*id)->identifier);
        if (prevDef != currBlock_->declMap.end()) {
            std::stringstream ss;
            ss << "redefinition of identifier "
                << static_cast<Tokens::IdentifierTk*>(&*id)->identifier;
            saveError(id->span, ss.str());

            reporter_.report({
                .level = CompileMsg::Level::Appendix,
                .message = "previously defined here:",
                .span = prevDef->second->span,
            });
            return;
        }
    }

    auto&& routineDecl = std::make_shared<Ast::RoutineDecl>(static_cast<Tokens::IdentifierTk*>(&*id)->identifier);
    auto&& tk = tokens_.get();
    if (!tk || (tk->type != TokenType::BRACKET_OPEN && tk->type != TokenType::ROUTINE_ARROW)) {
        saveError({
            .line = tk->span.line,
            .start = tk->span.start,
            .end = tk->span.start+1,
        }, "expected '(' or '=>' after routine identifier");
        return;
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
                    parseRoutineParam(routineDecl);
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
            return;
        }
        tokens_.move();

        if ((tk = tokens_.get()) != nullptr) {
            if (tk->type == TokenType::Is) {
                tokens_.move();
                // TODO parse body
            } else if (tk->type != TokenType::ENDLINE || tk->type != TokenType::SEMICOLON) {
                saveError({
                    .line = tk->span.line,
                    .start = tk->span.start,
                    .end = tk->span.start + 1,
                }, "expected 'is' before routine body");
                return;
            }
        }
    } else {
        // TODO =>
    }

    currBlock_->declarations.emplace_back(routineDecl);
    currBlock_->declMap.emplace(routineDecl->id, std::move(routineDecl));
}

void Parser::parseRoutineParam(std::shared_ptr<Ast::RoutineDecl>& parent) {
    std::cout << "= Parsing param\n";

    auto&& id = tokens_.get();
    if (!id || id->type != TokenType::Identifier) {
        saveError({
            .line = id->span.line,
            .start = id->span.start,
            .end = id->span.start+1,
        }, "expected parameter identifier");
        return;
    }
    tokens_.move();
    
    std::shared_ptr<Tokens::BaseTk> tk = tokens_.get();
    if (!tk || tk->type != TokenType::COLON) {
        saveError({
            .line = tk->span.line,
            .start = tk->span.start,
            .end = tk->span.start+1,
        }, "expected ':' after parameter identifier");
        return;
    }
    tokens_.move();
    
    auto&& type = parseType();
    if (!type) {
        saveError({
            .line = tk->span.line,
            .start = tk->span.start,
            .end = tk->span.start+1,
        }, "expected parameter type identifier");
        return;
    }
    tokens_.move();

    parent->params.emplace_back(std::make_shared<Ast::VarDecl>(static_cast<Tokens::IdentifierTk*>(&*tk)->identifier, std::move(type)));
}

std::shared_ptr<Ast::Type> Parser::parseType() {
    std::cout << "= Parsing type\n";

    auto&& tk = tokens_.get();
    if (!tk)
        return {nullptr};
    if (tk->type == TokenType::Identifier) {
        auto&& type = findNamedType(static_cast<Tokens::IdentifierTk*>(&*tk)->identifier);
        if (type)
            return std::shared_ptr(std::move(type));
    }

    // TODO custom types
    
    return {nullptr};
}