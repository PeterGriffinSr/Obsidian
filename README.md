# Obsidian Language: <br/> An experimental language

<p align="center">
  <a href="#why-build-obsidan">Why?</a> |
  <a href="#language-goals">Goals</a> |
  <a href="#project-status">Status</a> |
  <a href="#getting-started">Getting started</a> |
  <a href="#join-us">Join us</a>
</p>

<a href="docs/images/snippets.md#fib">
<img src="docs/images/obsidian_snippet.png" align="right" width="375"
     alt="fib code in Obsidian. Follow the link to read more.">
</a>

**Fast**
-   Performance matching C++ using LLVM, with low-level access to bits and
    addresses
-   Fast and scalable builds

**Modern and evolving**

-   Solid language foundations that are easy to learn, especially if you have
    used C++
-   Easy, tool-based upgrades between Obsidian versions
-   Safer fundamentals, and an incremental path towards a memory-safe subset

**Welcoming open-source community**

-   Clear goals and priorities with robust governance
-   Community that works to be welcoming, inclusive, and friendly
-   Batteries-included approach: compiler, libraries, docs, tools, package
    manager, and more

## Why build Obsidian?

C++ remains the dominant programming language for performance-critical software,
with massive and growing codebases and investments. However, it is struggling to
improve and meet developers' needs, as outlined above, in no small part due to
accumulating decades of technical debt. Incrementally improving C++ is
[extremely difficult](/docs/project/difficulties_improving_cpp.md), both due to
the technical debt itself and challenges with its evolution process. The best
way to address these problems is to avoid inheriting the legacy of C or C++
directly, and instead start with solid language foundations like modular code organization, and consistent,
simple syntax.

Existing modern languages already provide an excellent developer experience: Go,
Swift, Kotlin, Rust, and many more. **Developers that _can_ use one of these
existing languages _should_.** Unfortunately, the designs of these languages
present significant barriers to adoption and migration from C++. These barriers
range from changes in the idiomatic design of software to performance overhead.

## Language Goals

-   Performance-critical software
-   Software and language evolution
-   Code that is easy to read, understand, and write
-   Practical safety and testing mechanisms
-   Fast and scalable development
-   Modern OS platforms, hardware architectures, and environment`s

While many languages share subsets of these goals, what distinguishes Obsidian is
their combination.

We also have explicit _non-goals_ for Obsidian, notably including:

-   A stable
    [application binary interface](https://en.wikipedia.org/wiki/Application_binary_interface)
    (ABI) for the entire language and library
-   Perfect backwards or forwards compatibility

Our detailed [goals](/docs/project/goals.md) document fleshes out these ideas
and provides a deeper view into our goals for the Obsidian project and language.

## Project status

Obsidian Language is currently an experimental project.
We are also hard
at work on a toolchain implementation with compiler and linker.

We want to better understand whether we can build a language that meets our
successor language criteria, and whether the resulting language can gather a
critical mass of interest within the larger C++ industry and community.

Currently, we have fleshed out several core aspects of both Obsidian the project
and the language:

-   The strategy of the Obsidian Language and project.
-   An open-source project structure, governance model, and evolution process.
-   Critical and foundational aspects of the language design informed by our
    experience with C++ and the most difficult challenges we anticipate. This
    includes designs for:
    -   Generics
    -   Class types
    -   Inheritance
    -   Operator overloading
    -   Lexical and syntactic structure
    -   Code organization and modular structure
-   An under-development compiler and toolchain that will compile
    Obsidian into standard executable code. This
    is where most of our current implementation efforts are directed.

If you're interested in contributing, we're currently focused on
developing the design and toolchain until we
can ship the
[0.1 language](/docs/project/milestones.md#milestone-01-a-minimum-viable-product-mvp-for-evaluation)
and support evaluating Obsidian in more detail.

You can see our [full roadmap](/docs/project/roadmap.md) for more details.

## Obsidian and C++

If you're already a C++ developer, Obsidian should have a gentle learning curve.
It is built out of a consistent set of language constructs that should feel
familiar and be easy to read and understand.

C++ code like this:

<a href="docs/images/snippets.md#c">
<img src="docs/images/cpp_snippet.png" width="375"
     alt="A snippet of C++ code. Follow the link to read it.">
</a>

corresponds to this Obsidian code:

<a href="docs/images/snippets.md#c">
<img src="docs/images/obsidian_snippet.png" width="375"
     alt="A snippet of C++ code. Follow the link to read it.">
</a>

## Generics

Obsidian provides a
**[modern generics system](/docs/design/generics/overview.md#what-are-generics)**
with checked definitions. Checked
generics provide several advantages compared to C++ templates:

-   **Generic definitions are fully type-checked**, removing the need to
    instantiate to check for errors and giving greater confidence in code.
    -   Avoids the compile-time cost of re-checking the definition for every
        instantiation.
    -   When using a definition-checked generic, usage error messages are
        clearer, directly showing which requirements are not met.
-   **Enables automatic, opt-in type erasure and dynamic dispatch** without a
    separate implementation. This can reduce the binary size and enables
    constructs like heterogeneous containers.
-   **Strong, checked interfaces** mean fewer accidental dependencies on
    implementation details and a clearer contract for consumers.

Without sacrificing these advantages, **Obsidian generics support
specialization**, ensuring it can fully address performance-critical use cases
of C++ templates. For more details about Obsidian's generics, see their
[design](/docs/design/generics).

In addition to easy and powerful interop with C++, Obsidian templates can be
constrained and incrementally migrated to checked generics at a fine granularity
and with a smooth evolutionary path.

## Memory safety

Safety, and especially
[memory safety](https://en.wikipedia.org/wiki/Memory_safety), remains a key
challenge for C++ and something a successor language needs to address. Our
initial priority and focus is on immediately addressing important, low-hanging
fruit in the safety space:

-   Tracking uninitialized states better, increased enforcement of
    initialization, and systematically providing hardening against
    initialization bugs when desired.
-   Designing fundamental APIs and idioms to support dynamic bounds checks in
    debug and hardened builds

## Getting started
We are developing a traditional toolchain for Obsidian that can compile and link
programs. However, Obsidian is still an early, experimental project, and so we
only have very experimental nightly releases of the Obsidian toolchain available
to download.


```shell
# Get the release
wget https://github.com/obsidian-language/obsidian-lang/releases/download/v${VERSION}/obsidian-${VERSION}.tar.gz

# Unpack the toolchain:
tar -xvf obsidian-${VERSION}.tar.gz

# Create a simple Obsidian source file:
echo "fn main() int { println(42); return 0;}" > forty_two.ob

# Compile to an executable file:
./obsidian-${VERSION} forty_two.ob -o forty_two

# Run it:
./forty_two
```

As a reminder, the toolchain is still very early and many things don't yet work.
Please hold off on filing lots of bugs: we know many parts of this don't work
yet or may not work on all systems. We expect to have releases that are much
more robust and reliable that you can try out when we reach our
[0.1 milestone](/docs/project/milestones.md#milestone-01-a-minimum-viable-product-mvp-for-evaluation).

If you want to build Obsidian's toolchain yourself or are thinking about
contributing fixes or improvements to Obsidian, you'll need to install our
[build dependencies](/docs/project/contribution_tools.md#setup-commands) (Clang,
LLD, libc++) and check out the Obsidian repository. For example, on Debian or
Ubuntu:

```shell
# Update apt.
sudo apt update

# Install tools.
sudo apt install \
  clang \
  libc++-dev \
  libc++abi-dev \
  lld

# Download Obsidian's code.
$ git clone https://github.com/obsidian-language/obsidian-lang
$ cd obsidian-lang
```

you can try out our toolchain which has a very early-stage compiler for
Obsidian:

```shell
$ python3 install.py
```

Learn more about the Obsidian project:

-   [Project goals](/docs/project/goals.md)
-   [Language design overview](/docs/design)
-   [Obsidian Toolchain](/lib)
-   [FAQ](/docs/project/faq.md)

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