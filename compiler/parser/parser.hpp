#pragma once

#include "compiler/lexer/lexer.hpp"
#include "ast.hpp"
#include <vector>
#include <memory>

namespace Parser {
    class Parser {
    public:
        explicit Parser(std::vector<Lexer::Token>& tokens, Lexer::Lexer& lexer);

        std::shared_ptr<Ast::Expression> parse_expression(int precedence = 0);
        std::shared_ptr<Ast::Expression> parse_declaration();
        std::shared_ptr<Ast::Expression> parse();
        std::shared_ptr<Ast::Expression> parse_function();
        std::shared_ptr<Ast::Expression> parse_statement();
        std::shared_ptr<Ast::BlockStmt> parse_block();
        bool HasTokens() const;

    private:
        std::vector<Lexer::Token>& tokens;
        size_t current;
        Lexer::Lexer& lexer;
        const Lexer::Token& current_token() const;
        const Lexer::Token& next_token();
        bool match(Lexer::TokenType type);
        int get_precedence(const Lexer::Token& token) const;
    };
}