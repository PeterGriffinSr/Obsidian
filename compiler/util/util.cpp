#include "util.hpp"
#include "compiler/lexer/lexer.hpp"
#include "compiler/parser/parser.hpp"
#include <fstream>
#include <iostream>

std::string Utils::Flags::read(const std::string &path) {
    std::ifstream file(path);
    if (!file) {
        std::cerr << "Error opening file: " << path << std::endl;
        return "";
    }

    return std::string((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
}

void Utils::Flags::build(const std::string &path) {
    std::string source = read(path);
    if (source.empty()) {
        std::cerr << "Source is empty, cannot proceed." << std::endl;
        return;
    }

    Lexer::Lexer lexer(source);
    auto tokens = lexer.scanToken();
    if (tokens.empty()) {
        std::cerr << "Lexer produced no tokens." << std::endl;
        return;
    }

    Parser::Parser parser(tokens, lexer);
    while (parser.HasTokens()) {
        auto expr = parser.parse();
        if (expr == nullptr) {
            std::cerr << "Parser encountered an error while parsing." << std::endl;
            return;
        }
        Ast::printAst(expr);
    }
}