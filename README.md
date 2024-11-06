# Obsidian Language: 
**An experimental language**

<p align="center">
  <a href="#why-build-obsidian">Why?</a> |
  <a href="#language-goals">Goals</a> |
  <a href="#project-status">Status</a> |
  <a href="#getting-started">Getting started</a> |
  <a href="#join-us">Join us</a>
</p>

<a href="docs/images/snippets.md#fib">
<img src="docs/images/obsidian_snippet.png" align="right" width="375" alt="fib code in Obsidian. Follow the link to read more.">
</a>

### **Why Obsidian?**

- **Fast**:  
  - Performance comparable to C++ using LLVM, with direct access to low-level constructs.
  - Efficient and scalable builds.

- **Modern & Evolving**:  
  - Intuitive for developers familiar with C++, with easy-to-learn language foundations.
  - Smooth upgrade path between versions.
  - Emphasis on safety and a roadmap towards memory safety.

- **Welcoming Open-source Community**:  
  - Transparent goals and decision-making.
  - An inclusive, friendly, and open community.
  - Comprehensive toolset: compiler, libraries, documentation, package manager, and more.

## Why build Obsidian?

C++ is the go-to language for high-performance applications, but it's burdened with decades of technical debt, making improvements difficult. We believe that starting fresh with a new language—without the legacy baggage of C/C++—is the best way to overcome these challenges. Obsidian aims to offer a language with clean syntax, modular design, and modern features that make the transition from C++ easier without sacrificing performance.

Obsidian isn't meant to replace modern languages like Go, Swift, or Rust; it's meant for cases where migrating away from C++ is not feasible due to design or performance reasons.

## Language Goals

- High performance for critical applications.
- Code that's easy to read, write, and maintain.
- Safe coding patterns with built-in testing mechanisms.
- Support for modern OS platforms, architectures, and environments.
- Fast iteration and development cycles.

While these goals overlap with other languages, Obsidian's unique value lies in their combination.

### Non-goals:

- Stable [application binary interface (ABI)](https://en.wikipedia.org/wiki/Application_binary_interface).
- Absolute backward or forward compatibility.

For more details, see our [goals document](/docs/project/goals.md).

## Project Status

Obsidian is in the experimental phase, with active development on the toolchain, including a compiler and linker. Our focus is on creating a viable language for evaluation as a C++ successor. Key developments include:

- A robust strategy and structure for the Obsidian project.
- An open-source governance model and evolution process.
- Core language design informed by C++ experiences, covering:
  - Generics, class types, and inheritance.
  - Operator overloading.
  - Syntax and modular design.
- An early-stage compiler for converting Obsidian code into executables.

Check out our [full roadmap](/docs/project/roadmap.md) for more details.

## Obsidian vs. C++

<p align="center">
  <a href="docs/snippets.md#c" style="display: inline-block; margin-right: 10px;">
    <img src="docs/images/cpp_snippet.png" width="375" alt="A snippet of C++ code. Follow the link to read it." style="vertical-align: top;">
  </a>
  <a href="docs/snippets.md#obsidian" style="display: inline-block; margin-left: 10px;">
    <img src="docs/images/obsidian_snippet.png" width="375" alt="A snippet of Obsidian code. Follow the link to read it." style="vertical-align: top;">
  </a>
</p>

## Generics

Obsidian features a **[modern generics system](/docs/design/generics/overview.md#what-are-generics)** with type-checked definitions:

- **Type-checked generics**: Catch errors early, without needing instantiation.
- **Automatic type erasure**: Supports dynamic dispatch without separate implementations, reducing binary size.
- **Clear interfaces**: Reduce accidental dependencies and ensure clearer contracts.

Obsidian's generics system also allows **specialization** for performance-critical cases, offering similar flexibility to C++ templates. Learn more about [Obsidian's generics](/docs/design/generics).

## Memory Safety

Memory safety is a priority, focusing on:

- Improved tracking and enforcement of initialization.
- Dynamic bounds checks for secure debug builds.
- Robust API designs to support safer programming patterns.

## Getting Started

Obsidian is in early development, with nightly releases of the toolchain available.

```shell
# Get the release
wget https://github.com/obsidian-language/obsidian-lang/releases/download/v${VERSION}/obsidian-${VERSION}.tar.gz

# Unpack the toolchain:
tar -xvf obsidian-${VERSION}.tar.gz

# Create a simple Obsidian source file:
echo "fn main() int { println(42); return 0; }" > forty_two.ob

# Compile to an executable file:
./obsidian-${VERSION} forty_two.ob -o forty_two

# Run it:
./forty_two
```

Note: This is a very early-stage release, and many features are still in development. Check out our [0.1 milestone](/docs/project/milestones.md) for upcoming improvements.

For those interested in building from source or contributing, follow the setup instructions:
```shell
# Update apt.
sudo apt update

# Install tools.
sudo apt install \
  clang \
  libc++-dev \
  libc++abi-dev \
  lld

# Clone the Obsidian repository.
git clone https://github.com/obsidian-language/obsidian-lang
cd obsidian-lang
```

For those who just want the executable:
```shell
curl -fsSL https://raw.githubusercontent.com/PeterGriffinSr/ember/refs/heads/main/ember | bash
```

## Join us

We'd love to have folks join us and contribute to the project. Obsidian is
committed to a welcoming and inclusive environment where everyone can
contribute.

-   Most of Obsidian's design discussions occur on
    [Discord](https://discord.gg/#).
-   To watch for major release announcements, subscribe to our
    [Obsidian release post on GitHub](#)
    and [star Obsidian-lang](https://github.com/obsidian-language/obsidian-lang).
-   See our [code of conduct](CODE_OF_CONDUCT.md) and
    [contributing guidelines](CONTRIBUTING.md) for information about the Obsidian
    development community.

### Contributing

You can also directly:

-   [Contribute to the language design](CONTRIBUTING.md#contributing-to-the-language-design):
    feedback on design, new design proposal
-   [Contribute to the language implementation](CONTRIBUTING.md#contributing-to-the-language-implementation)
    -   [Obsidian Toolchain](/lib/), and project infrastructure

You can **check out some
["good first issues"](https://github.com/obsidian-language/obsidian-lang/labels/good%20first%20issue)**,
or join the `#contributing-help` channel on
[Discord](https://discord.gg/#). See our full
[`CONTRIBUTING`](CONTRIBUTING.md) documentation for more details.

<!-- Badges -->
[actions-windows-master]: https://github.com/PeterGriffinSr/Obsidian/workflows/Windows%20(master)/badge.svg
[actions-macos-master]: https://github.com/PeterGriffinSr/Obsidian/workflows/macOS/badge.svg?branch=master
[actions-linux-master]: https://github.com/PeterGriffinSr/Obsidian/workflows/Linux%20(master)/badge.svg
