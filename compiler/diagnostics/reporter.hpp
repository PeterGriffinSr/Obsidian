#pragma once

#include <cstdlib>
#include "compiler/lexer/lexer.hpp"

namespace diagnostics {
    enum class ExitValue {
        OK,
        INVALID_FILE_EXTENSION ,
        INVALID_FILE,
        LEXER_ERROR,
        PARSER_ERROR,
        _ERROR,
        MEMORY_ALLOCATION_FAILURE,
        INVALID_ARGUMENT,
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