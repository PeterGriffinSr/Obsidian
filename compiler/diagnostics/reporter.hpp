#pragma once

#include <cstdlib>
#include "compiler/lexer/lexer.hpp"

namespace diagnostics {
    enum class ExitValue {
        OK = 0,
        INVALID_FILE_EXTENSION = 1,
        INVALID_FILE = 2,
        LEXER_ERROR = 3,
        PARSER_ERROR = 4,
        _ERROR = 5,
        MEMORY_ALLOCATION_FAILURE = 6,
        INVALID_ARGUMENT = 7,
        VM_ERROR = 8,
        VM_RETURN = 9,
    };

    class Error {
    public:
        static void error(const Lexer::Token, const std::string& msg, Lexer::Lexer &lexer);
        static void memError(const std::string &msg);
        static void lexerError(const Lexer::Token token, const std::string &msg, Lexer::Lexer &lexer);
        static void parserError(const Lexer::Token token, const std::string &msg, Lexer::Lexer &lexer);

    private:
        inline static int errorCount = 0;
        static void printErrorMessage(const Lexer::Token token, const std::string &msg);
        static void printErrorLocation(const Lexer::Token token, Lexer::Lexer &lexer);
        static void Exit(ExitValue exitValue) { exit(static_cast<int>(exitValue)); }
    };
}