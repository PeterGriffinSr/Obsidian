#pragma once

#include "compiler/lexer/lexer.hpp"
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace Ast {
    struct BinaryExpression;
    struct FloatLiteral;
    struct NumberLiteral;
    struct Variable;
    struct VariableDeclaration;
    struct Parameter;
    struct FunctionDeclaration;
    struct ReturnStatement;
    struct BlockStmt;
    struct SwitchStmt;
    struct SwitchCase;
    struct StringLiteral;
    struct CharLiteral;
    struct BoolLiteral;

    using Expression = std::variant<BinaryExpression, FloatLiteral, NumberLiteral, Variable, VariableDeclaration, Parameter, FunctionDeclaration, ReturnStatement, BlockStmt, SwitchStmt, SwitchCase, StringLiteral, CharLiteral, BoolLiteral>;

    struct NumberLiteral {
        int value;
    };

    struct FloatLiteral {
        double value;
    };

    struct StringLiteral {
        std::string value;
    };

    struct CharLiteral {
        char value;
    };

    struct BoolLiteral {
        bool value;
    };

    struct Parameter {
        std::string name;
        std::optional<std::string> type;
    };

    struct FunctionDeclaration {
        std::string name;
        std::vector<Parameter> parameters;
        std::optional<std::string> returnType;
        std::shared_ptr<BlockStmt> body;
    };

    struct Variable {
        std::string name;
    };

    struct BinaryExpression {
        std::shared_ptr<Expression> left;
        std::shared_ptr<Expression> right;
        Lexer::TokenType op;
    };

    struct ReturnStatement {
        std::shared_ptr<Expression> expr;
    };

    struct BlockStmt {
        std::vector<std::shared_ptr<Expression>> statements;

        BlockStmt(const std::vector<std::shared_ptr<Expression>>& stmts) : statements(stmts) {}
    };

    struct VariableDeclaration {
        std::string name;
        std::shared_ptr<Expression> initializer;
        std::optional<std::string> type;
    };

    struct SwitchCase {
        std::shared_ptr<Expression> condition;
        std::shared_ptr<BlockStmt> body;
    };

    struct SwitchStmt {
        std::shared_ptr<Expression> condition;
        std::vector<SwitchCase> cases;
        std::shared_ptr<BlockStmt> default_case;
    };

    void printAst(const std::shared_ptr<Expression>& expr, int indent = 0);
}