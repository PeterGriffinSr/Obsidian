#include "reporter.hpp"
#include "termcolor.hpp"

void diagnostics::Error::error(const Lexer::Token token, const std::string &msg, Lexer::Lexer &lexer) {
    printErrorMessage(token, msg);
    printErrorLocation(token, lexer);

    if (errorCount == 5) {
        Exit(ExitValue::_ERROR);
    }
    errorCount++;
}

void diagnostics::Error::memError(const std::string &msg) {
    std::cout << termcolor::red << "Error" << termcolor::reset << ": "
    << termcolor::yellow << msg  << termcolor::reset << std::endl;
    Exit(ExitValue::MEMORY_ALLOCATION_FAILURE);
}

void diagnostics::Error::printErrorMessage(const Lexer::Token token, const std::string &msg) {
    std::cout << termcolor::red << "Error" << termcolor::reset << ": [line: "
    << termcolor::bright_blue << token.line << termcolor::reset
    << ", column: " << termcolor::bright_blue << token.column
    << termcolor::reset << "] " << termcolor::yellow << msg
    << termcolor::reset << std::endl;
}

void diagnostics::Error::printErrorLocation(const Lexer::Token token, Lexer::Lexer &lexer) {
    std::string lineContent = lexer.lineStart(token.line);
    size_t lineEnd = lineContent.find('\n');
    if (lineEnd == std::string::npos) {
        lineEnd = lineContent.size();
    }

    std::cout << "    " << token.line << " | " << lineContent.substr(0, lineEnd) << std::endl;
    std::cout << "      |" << std::string(token.column, ' ') << termcolor::red<< "^" << termcolor::reset << std::endl;
}

void diagnostics::Error::lexerError(const Lexer::Token token, const std::string &msg, Lexer::Lexer &lexer) {
    printErrorMessage(token, msg);
    printErrorLocation(token, lexer);
    errorCount++;

    if (errorCount == 1) {
        Exit(ExitValue::LEXER_ERROR);
    }
}

void diagnostics::Error::parserError(const Lexer::Token token, const std::string &msg, Lexer::Lexer &lexer) {
    printErrorMessage(token, msg);
    printErrorLocation(token, lexer);
    errorCount++;

    if (errorCount == 1) {
        Exit(ExitValue::PARSER_ERROR);
    }
}