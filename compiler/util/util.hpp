#pragma once

#include <string>

namespace Utils {
    class Flags {
    public:
        static std::string read(const std::string &path);
        static void build(const std::string& path);
    };
}