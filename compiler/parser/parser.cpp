#include "parser.hpp"
#include "compiler/diagnostics/reporter.hpp"
#include "compiler/lexer/lexer.hpp"
#include "compiler/parser/ast.hpp"
#include <memory>
#include <string>

Parser::Parser::Parser(std::vector<Lexer::Token>& tokens, Lexer::Lexer& lexer)
    : tokens(tokens), current(0), lexer(lexer) {}

const Lexer::Token& Parser::Parser::current_token() const {
    return tokens[current];
}

const Lexer::Token& Parser::Parser::next_token() {
    return tokens[++current];
}

bool Parser::Parser::HasTokens() const {
    return current < tokens.size() && tokens[current].type != Lexer::TokenType::Eof;
}

bool Parser::Parser::match(Lexer::TokenType type) {
    if (current_token().type == type) {
        next_token();
        return true;
    }
    return false;
}

int Parser::Parser::get_precedence(const Lexer::Token& token) const {
    switch (token.type) {
        case Lexer::TokenType::Plus:
        case Lexer::TokenType::Minus:
            return 1;
        case Lexer::TokenType::Star:
        case Lexer::TokenType::Slash:
            return 2;
        case Lexer::TokenType::Power:
            return 3;
        default:
            return 0;
    }
}

std::shared_ptr<Ast::Expression> Parser::Parser::parse_expression(int precedence) {
    const Lexer::Token& token = current_token();
    std::shared_ptr<Ast::Expression> left;

    switch (token.type) {
        case Lexer::TokenType::Int: {
            int value = std::stod(token.value);
            left = std::make_shared<Ast::Expression>(Ast::NumberLiteral{(value)});
            next_token();
            break;
        }
        case Lexer::TokenType::Float: {
            double value = std::stod(token.value);
            left = std::make_shared<Ast::Expression>(Ast::FloatLiteral{(value)});
            next_token();
            break;
        
        }
        case Lexer::TokenType::String: {
            std::string value = token.value;
            left = std::make_shared<Ast::Expression>(Ast::StringLiteral{(value)});
            next_token();
            break;
        }
        case Lexer::TokenType::Char: {
            char value = token.value[0];
            left = std::make_shared<Ast::Expression>(Ast::CharLiteral{(value)});
            next_token();
            break;
        }
        case Lexer::TokenType::Bool: {
            bool value = (token.value == "true");
            left = std::make_shared<Ast::Expression>(Ast::BoolLiteral{value});
            next_token();
            break;
        }
        case Lexer::TokenType::Idenifier: {
            left = std::make_shared<Ast::Expression>(Ast::Variable{token.value});
            next_token();
            break;
        }
        default:
            diagnostics::Error::parserError(token, "Unexpected token in expression", lexer);
            return nullptr;
    }

    while (true) {
        const Lexer::Token& next = current_token();
        int next_precedence = get_precedence(next);

        if (next_precedence <= precedence) {
            break;
        }

        next_token();
        auto right = parse_expression(next_precedence);

        left = std::make_shared<Ast::Expression>(Ast::BinaryExpression{
            left, right, next.type
        });
    }

    return left;
}

std::shared_ptr<Ast::Expression> Parser::Parser::parse_declaration() {
    if (!match(Lexer::TokenType::Let)) {
        diagnostics::Error::parserError(current_token(), "Expected 'let' keyword", lexer);
        return nullptr;
    }

    const Lexer::Token& identifier_token = current_token();
    if (!match(Lexer::TokenType::Idenifier)) {
        diagnostics::Error::parserError(identifier_token, "Expected identifier after 'let'", lexer);
        return nullptr;
    }

    std::string var_name = identifier_token.value;
    std::optional<std::string> type;

    if (match(Lexer::TokenType::Colon)) {
        if (match(Lexer::TokenType::IntType)) {
            type = "int";
        } else if (match(Lexer::TokenType::FloatType)) {
            type = "float";
        } else if (match(Lexer::TokenType::CharType)) {
            type = "char";
        } else if (match(Lexer::TokenType::BoolType)) {
            type = "bool";
        } else if (match(Lexer::TokenType::StringType)) {
            type = "string";
        } else {
            diagnostics::Error::parserError(current_token(), "Expected a valid type after ':'", lexer);
            return nullptr;
        }
    }

    if (!(match(Lexer::TokenType::Assign) || match(Lexer::TokenType::ImplAssign))) {
        diagnostics::Error::parserError(current_token(), "Expected '=' or ':=' after variable declaration", lexer);
        return nullptr;
    }

    auto expr = parse_expression();
    return std::make_shared<Ast::Expression>(Ast::VariableDeclaration{var_name, expr, type});
}

std::shared_ptr<Ast::Expression> Parser::Parser::parse_statement() {
    if (match(Lexer::TokenType::Return)) {
        auto expr = parse_expression();
        if (!expr) {
            diagnostics::Error::parserError(current_token(), "Expected expression after 'return'", lexer);
            return nullptr;
        }
        return std::make_shared<Ast::Expression>(Ast::ReturnStatement{expr});
    } else if (current_token().type == Lexer::TokenType::Let) {
        return parse_declaration();
    } else {
        return parse_expression();
    }
}

std::shared_ptr<Ast::BlockStmt> Parser::Parser::parse_block() {
    if (!match(Lexer::TokenType::LBrace)) {
        diagnostics::Error::parserError(current_token(), "Expected '{' to start block", lexer);
        return nullptr;
    }

    std::vector<std::shared_ptr<Ast::Expression>> statements;

    while (current_token().type != Lexer::TokenType::RBrace && current_token().type != Lexer::TokenType::Eof) {
        auto stmt = parse_statement();
        if (!stmt) {
            return nullptr;
        }
        statements.push_back(stmt);
    }

    if (!match(Lexer::TokenType::RBrace)) {
        diagnostics::Error::parserError(current_token(), "Expected '}' to end block", lexer);
        return nullptr;
    }

    return std::make_shared<Ast::BlockStmt>(statements);
}

std::shared_ptr<Ast::Expression> Parser::Parser::parse_function() {
    if (!match(Lexer::TokenType::Fun)) {
        diagnostics::Error::parserError(current_token(), "Expected 'fun' keyword", lexer);
        return nullptr;
    }

    const Lexer::Token& identifier_token = current_token();
    if (!match(Lexer::TokenType::Idenifier)) {
        diagnostics::Error::parserError(identifier_token, "Expected function name after 'fun'", lexer);
        return nullptr;
    }

    std::string func_name = identifier_token.value;
    std::vector<Ast::Parameter> parameters;

    if (!match(Lexer::TokenType::LParen)) {
        diagnostics::Error::parserError(current_token(), "Expected '(' after function name", lexer);
        return nullptr;
    }

    while (current_token().type != Lexer::TokenType::RParen) {
        const Lexer::Token& param_token = current_token();
        if (!match(Lexer::TokenType::Idenifier)) {
            diagnostics::Error::parserError(param_token, "Expected parameter name", lexer);
            return nullptr;
        }

        std::string param_name = param_token.value;
        std::optional<std::string> param_type;

        if (match(Lexer::TokenType::Colon)) {
            if (match(Lexer::TokenType::IntType)) {
                param_type = "int";
            } else if (match(Lexer::TokenType::FloatType)) {
                param_type = "float";
            } else {
                diagnostics::Error::parserError(current_token(), "Expected a valid type for parameter", lexer);
                return nullptr;
            }
        }

        parameters.push_back(Ast::Parameter{param_name, param_type});

        if (!match(Lexer::TokenType::Comma) && current_token().type != Lexer::TokenType::RParen) {
            diagnostics::Error::parserError(current_token(), "Expected ',' or ')' after parameter", lexer);
            return nullptr;
        }
    }

    match(Lexer::TokenType::RParen);

    std::optional<std::string> returnType;

    if (match(Lexer::TokenType::Colon)) {
        if (match(Lexer::TokenType::IntType)) {
            returnType = "int";
        } else if (match(Lexer::TokenType::FloatType)) {
            returnType = "float";
        } else if (match(Lexer::TokenType::CharType)) {
            returnType = "char";
        } else if (match(Lexer::TokenType::BoolType)) {
            returnType = "bool";
        } else if (match(Lexer::TokenType::StringType)) {
            returnType = "string";
        } else {
            diagnostics::Error::parserError(current_token(), "Expected a valid return type", lexer);
            return nullptr;
        }
    }

    auto body = parse_block();
    if (!body) {
        return nullptr;
    }

    return std::make_shared<Ast::Expression>(Ast::FunctionDeclaration{func_name, parameters, returnType, body});
}

std::shared_ptr<Ast::Expression> Parser::Parser::parse_switch_statement() {
    if (!match(Lexer::TokenType::Switch)) {
        diagnostics::Error::parserError(current_token(), "Expected 'switch' keyword", lexer);
        return nullptr;
    }

    if (!match(Lexer::TokenType::LParen)) {
        diagnostics::Error::parserError(current_token(), "Expected '(' after 'switch'", lexer);
        return nullptr;
    }

    auto switch_expr = parse_expression();
    if (!switch_expr) {
        diagnostics::Error::parserError(current_token(), "Expected expression after 'switch ('", lexer);
        return nullptr;
    }

    if (!match(Lexer::TokenType::RParen)) {
        diagnostics::Error::parserError(current_token(), "Expected ')' after switch expression", lexer);
        return nullptr;
    }

    if (!match(Lexer::TokenType::LBrace)) {
        diagnostics::Error::parserError(current_token(), "Expected '{' after switch expression", lexer);
        return nullptr;
    }

    std::vector<Ast::SwitchCase> cases;
    std::shared_ptr<Ast::BlockStmt> default_case = nullptr;

    while (current_token().type != Lexer::TokenType::RBrace && current_token().type != Lexer::TokenType::Eof) {
        if (match(Lexer::TokenType::Case)) {
            auto case_condition = parse_expression();
            if (!case_condition) {
                diagnostics::Error::parserError(current_token(), "Expected expression after 'case'", lexer);
                return nullptr;
            }

            if (!match(Lexer::TokenType::Colon)) {
                diagnostics::Error::parserError(current_token(), "Expected ':' after case expression", lexer);
                return nullptr;
            }

            std::shared_ptr<Ast::BlockStmt> case_body;
            if (current_token().type == Lexer::TokenType::LBrace) {
                case_body = parse_block();
            } else {
                auto single_stmt = parse_statement();
                if (!single_stmt) {
                    diagnostics::Error::parserError(current_token(), "Expected statement after case ':'", lexer);
                    return nullptr;
                }
                case_body = std::make_shared<Ast::BlockStmt>(std::vector<std::shared_ptr<Ast::Expression>>{single_stmt});
            }

            cases.push_back(Ast::SwitchCase{case_condition, case_body});
        } else if (match(Lexer::TokenType::Default)) {
            if (!match(Lexer::TokenType::Colon)) {
                diagnostics::Error::parserError(current_token(), "Expected ':' after 'default'", lexer);
                return nullptr;
            }

            if (current_token().type == Lexer::TokenType::LBrace) {
                default_case = parse_block();
            } else {
                auto single_stmt = parse_statement();
                if (!single_stmt) {
                    diagnostics::Error::parserError(current_token(), "Expected statement after default ':'", lexer);
                    return nullptr;
                }
                default_case = std::make_shared<Ast::BlockStmt>(std::vector<std::shared_ptr<Ast::Expression>>{single_stmt});
            }
        } else {
            diagnostics::Error::parserError(current_token(), "Unexpected token in switch statement", lexer);
            return nullptr;
        }
    }

    if (!match(Lexer::TokenType::RBrace)) {
        diagnostics::Error::parserError(current_token(), "Expected '}' to end switch statement", lexer);
        return nullptr;
    }

    return std::make_shared<Ast::Expression>(Ast::SwitchStmt{switch_expr, cases, default_case});
}

std::shared_ptr<Ast::Expression> Parser::Parser::parse() {
    if (current_token().type == Lexer::TokenType::Let) {
        return parse_declaration();
    } else if (current_token().type == Lexer::TokenType::Fun) {
        return parse_function();
    } else if (current_token().type == Lexer::TokenType::Switch) {
        return parse_switch_statement();
    } else {
        return parse_expression();
    }
}