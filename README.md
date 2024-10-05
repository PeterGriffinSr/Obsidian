# Obsidian
A memory safe compiled programming language built in OCaml.

- [Features](#features)
- [Installation](#installation)
    - [Build](#)
- [Usage](#usage)
- [Examples](#examples)
    - [Hello, World](#hello-world)
- [Contribute](#contributing)
- [License](#license)

# Features
- compiled: Obsidian provides a compiled abstraction, allowing developers to write code that is closer to human-readable language.
    
- Static Typing: Variables in Obsidian are statically typed, meaning their types are pre-defined.
    
- File Extension: Obsidian source files have the extension `.ob`.

# Installation
To start using Obsidian, follow the installation guide below:

```
dune build
```

# Usage
```
./_build/default/bin/obsidian <filename>
```

# Examples

## Hello, world
```
fn main() {
    print("Hello, world!");
    return 0;
}
```

# Contributing
If you find any bugs or want to contribute to Obsidian, please feel free to open an issue or submit a pull request on the GitHub repository. We welcome your feedback and suggestions to make this tool even better.

# License
Obsidian is open-source software licensed under the [BSD 3-Clause](LICENSE) License. You are free to use, modify, and distribute this software with proper attribution and in compliance with the license terms.