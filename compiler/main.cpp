#include "compiler/util/util.hpp"
#include <iostream>

int main(int argc, char *argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <source file>" << std::endl;
        return 1;
    }

    Utils::Flags::build(argv[1]);

    return 0;
}