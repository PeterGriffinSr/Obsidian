# Obsidian
A memory safe compiled programming language built in C++.

- [Features](#features)
- [Installation](#installation)
    - [Build]
- [Usage](#usage)
- [Examples](#examples)
    - [Hello, World](#hello-world)
    - [Error Handling](#error-handling)
- [Contribute](#contributing)
- [License](#license)

# Features
- compiled: Obsidian provides a compiled abstraction, allowing developers to write code that is closer to human-readable language.
    
- Dynamic Typing/Static Typing: Variables in Obsidian are dynamically/statically typed, meaning their types are determined at runtime or pre-defined.
    
- File Extension: Obsidian source files have the extension `.ob`.

# Installation
To start using Obsidian, follow the installation guide below:

```
meson setup build && meson compile -C build
```

# Usage
```
./build/obsidian <filename>
```

# Examples

## Hello, world
```
fun main() {
    print("Hello, world!");
    exit(0);
}
```

## Error Handling
```
Error: [line: 2, column: 30] Unexpected character: ~
    2 |     print("Hello, world!\n");~
      |                              ^

```

# Contributing
If you find any bugs or want to contribute to Obsidian, please feel free to open an issue or submit a pull request on the GitHub repository. We welcome your feedback and suggestions to make this tool even better.

# License
Obsidian is open-source software licensed under the [BSD 3-Clause](LICENSE) License. You are free to use, modify, and distribute this software with proper attribution and in compliance with the license terms.