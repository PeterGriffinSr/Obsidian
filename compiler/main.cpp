#include "compiler/parser/ast.hpp"
#include "compiler/parser/parser.hpp"
#include "compiler/lexer/lexer.hpp"
#include "compiler/util/util.hpp"
#include <iostream>

int main(int argc, char *argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <source file>" << std::endl;
        return 1;
    }

    
    std::string source = Utils::Flags::read(argv[1]);
    Lexer::Lexer lexer(source);
    auto tokens = lexer.scanToken();

    Parser::Parser parser(tokens, lexer);
     while (parser.HasTokens()) {
        auto expr = parser.parse();
        if (expr == nullptr) {
            return 1;
        }
        Ast::printAst(expr);
    }

    // for (const auto& token : tokens) {
    //     std::cout << "Token: " << static_cast<int>(token.type) << ", Value: " << token.value << std::endl;
    // }

    return 0;
}