#pragma once

#include <cstddef>
#include <fstream>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace Lexer {
    enum class TokenType {
        LParen, RParen, LBrace, RBrace, LBracket, RBracket, Comma, Dot, Semi, Colon,
        Plus, Minus, Star, Slash, Percent, Not, Neq, Assign, Eq, Greater, Geq, Less, 
        Leq,  Ampersand, Carot, LogicalAnd, LogicalOr, Inc, Dec, Nullish, Idenifier, 
        String, Int, Char, Bool, Float, Nil, Fun, Let, Const, Return, If, Else, Switch, 
        Case, While, For, Import, Export, Default, Eof, Err, In, DotDot, ImplAssign, IntType,
        StringType, BoolType, CharType, FloatType, Class, Power
    };

    struct Token {
        TokenType type;
        std::string value;
        int line, column;

        Token(TokenType t, const std::string& v, int ln, int col)
            : type(t), value(v), line(ln), column(col) {}
    };

    class Lexer {
    public:
        explicit Lexer(const std::string& input);
        explicit Lexer(std::ifstream& file);

        std::optional<Token> nextToken();
        std::vector<Token> scanToken();
        std::string lineStart(int line) const;

    private:
        std::string input;
        size_t position;
        int currentLine, currentColumn;

        char nextChar();
        char peek(int offset = 0) const;
        std::optional<Token> scanIdentifier();
        std::optional<Token> scanString();
        std::optional<Token> scanNumber();
        void skipWhitespace();
        void skipSingleLineComment();
        void skipMultiLineComment();
        void reportError(const std::string& message);

        static const std::unordered_map<std::string, TokenType> keywords;
        static const std::unordered_map<std::string, TokenType> multiCharTokens;
        static const std::unordered_map<char, TokenType> singleCharTokens;
    };
}