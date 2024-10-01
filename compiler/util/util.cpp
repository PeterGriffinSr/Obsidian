#include "util.hpp"
#include <fstream>
#include <iostream>

std::string Utils::Flags::read(const std::string &path) {
    std::ifstream file(path);
    if (!file) {
        std::cerr << "Error opening file: " << path << std::endl;
        return "";
    }
    
    std::string content((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    return content;
}