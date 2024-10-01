#include "ast.hpp"
#include <iostream>

namespace Ast {
    void printAst(const std::shared_ptr<Expression>& expr, int indent) {
        std::string indentation(indent, ' ');

        if (std::holds_alternative<NumberLiteral>(*expr)) {
            const NumberLiteral& num = std::get<NumberLiteral>(*expr);
            std::cout << indentation << "NumberLiteral: " << num.value << std::endl;

        } else if (std::holds_alternative<Variable>(*expr)) {
            const Variable& var = std::get<Variable>(*expr);
            std::cout << indentation << "Variable: " << var.name << std::endl;

        } else if (std::holds_alternative<BinaryExpression>(*expr)) {
            const BinaryExpression& bin_expr = std::get<BinaryExpression>(*expr);
            std::cout << indentation << "BinaryExpression: " << std::endl;
            std::cout << indentation << "  Operator: " << static_cast<int>(bin_expr.op) << std::endl;
            std::cout << indentation << "  Left:" << std::endl;
            printAst(bin_expr.left, indent + 4);
            std::cout << indentation << "  Right:" << std::endl;
            printAst(bin_expr.right, indent + 4);

        } else if (std::holds_alternative<VariableDeclaration>(*expr)) {
            const VariableDeclaration& var_decl = std::get<VariableDeclaration>(*expr);
            std::cout << indentation << "VariableDeclaration: " << var_decl.name << std::endl;

            if (var_decl.type.has_value()) {
                std::cout << indentation << "  Type: " << var_decl.type.value() << std::endl;
            } else {
                std::cout << indentation << "  Type: Determined at runtime" << std::endl;
            }

            std::cout << indentation << "  Initializer:" << std::endl;
            printAst(var_decl.initializer, indent + 4);

        } else if (std::holds_alternative<FunctionDeclaration>(*expr)) {
            const FunctionDeclaration& func_decl = std::get<FunctionDeclaration>(*expr);
            std::cout << indentation << "FunctionDeclaration: " << func_decl.name << std::endl;

            std::cout << indentation << "  Parameters:" << std::endl;
            for (const auto& param : func_decl.parameters) {
                std::cout << indentation << "    " << param.name;
                if (param.type.has_value()) {
                    std::cout << ": " << param.type.value();
                } else {
                    std::cout << ": Inferred";
                }
                std::cout << std::endl;
            }

            if (func_decl.returnType.has_value()) {
                std::cout << indentation << "  Return Type: " << func_decl.returnType.value() << std::endl;
            } else {
                std::cout << indentation << "  Return Type: Inferred" << std::endl;
            }

            std::cout << indentation << "  Body:" << std::endl;
            if (func_decl.body) {
                printAst(std::make_shared<Expression>(*func_decl.body), indent + 4);
            } else {
                std::cout << indentation << "    <empty>" << std::endl;
            }

        } else if (std::holds_alternative<ReturnStatement>(*expr)) {
            const ReturnStatement& ret_stmt = std::get<ReturnStatement>(*expr);
            std::cout << indentation << "ReturnStatement:" << std::endl;
            printAst(ret_stmt.expr, indent + 4);

        } else if (std::holds_alternative<BlockStmt>(*expr)) {
            const BlockStmt& block = std::get<BlockStmt>(*expr);
            std::cout << indentation << "BlockStmt:" << std::endl;

            for (const auto& statement : block.statements) {
                printAst(statement, indent + 4);
            }
        }
    }
}