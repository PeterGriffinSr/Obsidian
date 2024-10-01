#include "lexer.hpp"
#include <sstream>
#include <unordered_map>    
#include "compiler/diagnostics/reporter.hpp"

const std::unordered_map<std::string, Lexer::TokenType> Lexer::Lexer::keywords = {
    {"fun", TokenType::Fun}, {"let", TokenType::Let}, {"switch", TokenType::Switch}, {"case", TokenType::Case}, {"int", TokenType::IntType},
    {"float", TokenType::FloatType}, {"string", TokenType::StringType}, {"char", TokenType::CharType}, {"const", TokenType::Const}, 
    {"nil", TokenType::Nil}, {"bool", TokenType::BoolType}, {"if", TokenType::If}, {"else", TokenType::Else}, 
    {"while", TokenType::While}, {"for", TokenType::For}, {"import", TokenType::Import}, {"export", TokenType::Export}, 
    {"in", TokenType::In}, {"default", TokenType::Default}, {"class", TokenType::Class}
};

const std::unordered_map<char, Lexer::TokenType> Lexer::Lexer::singleCharTokens = {
    {'(', TokenType::LParen}, {')', TokenType::RParen}, {'{', TokenType::LBrace},
    {'}', TokenType::RBrace}, {'[', TokenType::LBracket}, {']', TokenType::RBracket},
    {',', TokenType::Comma}, {'+', TokenType::Plus}, {'-', TokenType::Minus},
    {'*', TokenType::Star}, {'/', TokenType::Slash}, {';', TokenType::Semi},
    {':', TokenType::Colon}, {'.', TokenType::Dot}, {'!', TokenType::Not},
    {'=', TokenType::Assign}, {'>', TokenType::Greater}, {'<', TokenType::Less}
};

const std::unordered_map<std::string, Lexer::TokenType> Lexer::Lexer::multiCharTokens = {
    {"!=", TokenType::Neq}, {"==", TokenType::Eq}, {">=", TokenType::Geq}, {"<=", TokenType::Leq}, {"++", TokenType::Inc}, 
    {"--", TokenType::Dec}, {"&&", TokenType::LogicalAnd}, {"||", TokenType::LogicalOr}, {":=", TokenType::ImplAssign}, 
    {"..", TokenType::DotDot}
};

Lexer::Lexer::Lexer(const std::string& input) : input(input), position(0), currentLine(1), currentColumn(1) {}

Lexer::Lexer::Lexer(std::ifstream& file) {
    std::ostringstream ss;
    ss << file.rdbuf();
    input = ss.str();
    position = 0;
    currentLine = 1;
    currentColumn = 1;
}

void Lexer::Lexer::skipSingleLineComment() {
    while (peek() != '\n' && peek() != '\0') {
        nextChar();
    }
    nextChar();
}

void Lexer::Lexer::skipMultiLineComment() {
    while (true) {
        if (peek() == '\0') {
            reportError("Unterminated multi-line comment.");
            return;
        }

        if (peek() == '*' && peek(1) == '/') {
            nextChar();
            nextChar();
            break;
        }

        nextChar();
    }
}

std::vector<Lexer::Token> Lexer::Lexer::scanToken() {
    std::vector<Token> tokens;

    while (position < input.length()) {
        skipWhitespace();
        if (position >= input.length()) break;

        char current_char = peek();
        if (current_char == '/' && peek(1) == '/') {
            skipSingleLineComment();
            continue;
        }
        if (current_char == '/' && peek(1) == '*') {
            skipMultiLineComment();
            continue;
        }
        std::string two_char_token = std::string(1, current_char) + std::string(1, peek(1));
        if (multiCharTokens.count(two_char_token)) {
            tokens.push_back({multiCharTokens.at(two_char_token), two_char_token, currentLine, currentColumn});
            nextChar();
            nextChar();                     
            continue;
        }  
        if (singleCharTokens.count(current_char)) {
            tokens.push_back({singleCharTokens.at(current_char), std::string(1, current_char), currentLine, currentColumn});
            nextChar();
            continue;
        }     
        if (current_char == '"') {
            auto string_token = scanString();
            if (string_token) tokens.push_back(*string_token);
            continue;
        }
        if (std::isalpha(current_char)) {
            auto identifier_token = scanIdentifier();
            if (identifier_token) tokens.push_back(*identifier_token);
            continue;
        }
        if (std::isdigit(current_char)) {
            auto number_token = scanNumber();
            if (number_token) tokens.push_back(*number_token);
            continue;
        }
        reportError("Unexpected character.");
        tokens.push_back({TokenType::Err, std::string(1, nextChar()), currentLine, currentColumn});
    }
    tokens.push_back({TokenType::Eof, "EOF", currentLine, currentColumn});
    return tokens;
}


std::optional<Lexer::Token> Lexer::Lexer::nextToken() {
    auto tokens = scanToken();
    if (!tokens.empty()) {
        return tokens.front();
    }
    return std::nullopt;
}

char Lexer::Lexer::nextChar() {
    char current_char = input[position++];
    if (current_char == '\n') {
        currentLine++;
        currentColumn = 1;
    } else {
        currentColumn++;
    }
    return current_char;
}

char Lexer::Lexer::peek(int offset) const {
    if (position + offset >= input.length()) return '\0';
    return input[position + offset];
}

void Lexer::Lexer::skipWhitespace() {
    while (std::isspace(peek())) {
        nextChar();
    }
}

std::optional<Lexer::Token> Lexer::Lexer::scanIdentifier() {
    std::string result;
    int start_line = currentLine;
    int start_column = currentColumn;

    while (std::isalnum(peek()) || peek() == '_') {
        result += nextChar();
    }

    if (keywords.count(result)) {
        return Token{keywords.at(result), result, start_line, start_column};
    }

    return Token{TokenType::Idenifier, result, start_line, start_column};
}

std::optional<Lexer::Token> Lexer::Lexer::scanNumber() {
    std::string result;
    int start_line = currentLine;
    int start_column = currentColumn;

    while (std::isdigit(peek())) {
        result += nextChar();
    }

    if (peek() == '.' && std::isdigit(peek(1))) {
        result += nextChar();
        while (std::isdigit(peek())) {
            result += nextChar();
        }

        if (peek() == 'e' || peek() == 'E') {
            result += nextChar();
            if (peek() == '+' || peek() == '-') {
                result += nextChar();
            }

            while (std::isdigit(peek())) {
                result += nextChar();
            }
        }

        return Token{TokenType::Float, result, start_line, start_column};
    }

    return Token{TokenType::Int, result, start_line, start_column};
}

std::optional<Lexer::Token> Lexer::Lexer::scanString() {
    std::string result;
    int start_line = currentLine;
    int start_column = currentColumn;

    nextChar();

    while (true) {
        char current_char = peek();
        if (current_char == '\0' || current_char == '\n') {
            reportError("Unterminated string.");
            return Token{TokenType::Err, "Unterminated string.", start_line, start_column};
        }

        if (current_char == '"') {
            nextChar();
            return Token{TokenType::String, result, start_line, start_column};
        }

        result += nextChar();
    }
}

void Lexer::Lexer::reportError(const std::string& message) {
    Token errorToken(TokenType::Err, "", currentLine, currentColumn);
    diagnostics::Error::lexerError(errorToken, message, *this);
}

std::string Lexer::Lexer::lineStart(int line) const {
    if (line < 1) return "";

    size_t line_start = 0;
    int current_line = 1;

    for (size_t i = 0; i < input.length(); ++i) {
        if (current_line == line) {
            line_start = i; 
            break;
        }
        if (input[i] == '\n') {
            current_line++;
        }
    }

    if (current_line < line) return "";
    size_t line_end = input.find('\n', line_start);
    if (line_end == std::string::npos) line_end = input.length();

    return input.substr(line_start, line_end - line_start);
}