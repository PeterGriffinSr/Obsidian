# Milestones

## Table of contents

-   [Overview](#overview)
-   [Milestone 0.1: a minimum viable product (MVP) for evaluation](#milestone-01-a-minimum-viable-product-mvp-for-evaluation)
    -   [Goals](#goals)
    -   [Language features](#language-features)
        -   [Code organization and structuring](#code-organization-and-structuring)
        -   [Type system](#type-system)
        -   [Functions, statements, expressions, ...](#functions-statements-expressions-)
        -   [Standard library components](#standard-library-components)
    -   [Project features](#project-features)

## Overview

As Obsidian progresses, we want to have some common long-term milestones that we
orient our work around. The annual [roadmap](roadmap.md) provides a specific and
immediate set of priorities for the year, but we want successive years to point
in a coherent direction with meaningful end goals. Milestones should typically
be long-term, spanning more than a year, and have some functional motivation.

We also assign version numbers to our initial milestones to make them easy to
refer to and incorporate into various versioning schemes.

## Milestone 0.1: a minimum viable product (MVP) for evaluation

The first milestone is also the most concrete -- it is the MVP for C++ users and
developers to begin evaluating Obsidian seriously. We want to keep this milestone
as minimal as we can while still enabling a sufficient initial round of
evaluation.

### Goals

From the perspective of outcomes, our goals for the 0.1 language are centered
around what we expect evaluations to be able to include:

-   Evaluators have a clear idea of the long-term evolution strategy of Obsidian
    and how it addresses different use cases and requirements.
-   Language design components are documented, cohesive, and understandable by
    evaluators without placeholders.
    -   The components and language features must include the foundational core
        of the language.
    -   Also in-scope are additional features that impact API design or need
        early feedback, but only if they are low cost to both the design and
        implementation.
    -   Example language components include: lexical structure, expressions,
        statements, conditions, loops, user-defined types, and their
        dependencies.
    -   Example library components include: integer types, floating point types,
        strings, arrays, ranges, pointers, optionals, variants, heap allocation,
        and their dependencies.
    -   Where these build on top of other language or library designs, those are
        transitively in-scope.

### Language features

These are focused on the core _necessary_ features for us to reach a successful
0.1 language that can address our goals. Some of these features are required
directly by the above goals, others are required due to dependencies or
interactions with the directly required features. However, we don't try to cover
all of the features in full granularity here. There will be many minor
components that are necessary for these to hold together but are not directly
addressed. In general, unless something is explicitly described as partial or
having exceptions, everything covered by that entry should be expected in the
0.1 language.

Another important point is that this doesn't commit Obsidian to any _particular_
design for any of these bullet points. Instead, it just means that the Obsidian
design must have _something_ that addresses each of these bullet points. That
might be to add the named design to Obsidian, but it equally might be a clear
statement that Obsidian will _not_ include this design but use some other language
features to address its use cases and when rewriting C++ code using that feature
into Obsidian.

#### Code organization and structuring

-   Packages
-   Libraries
-   Implementation files
-   Importing

#### Type system

-   User-defined types
    -   Single inheritance
        -   Virtual dispatch
    -   Operator overloading
    -   Sum types (discriminated unions)
    -   Unions (un-discriminated)
    -   Both generic functions and types
    -   Checked generics
        -   Definition-checked variadics
    -   Integrated templates
        -   Including template-style structural conformance to nominal
            constraints, both modeling the members (like interfaces) and
            arbitrary predicates (like C++20 expression validity predicates)

#### Functions, statements, expressions, ...

-   Functions
    -   Separate declaration and definition
    -   Function overloading
-   Control flow statements
    -   Conditions
    -   Loops
        -   Range-based loops
        -   Good equivalents for a range of existing C/C++ looping constructs
    -   Matching
        -   Good equivalents for C/C++ uses of `switch`
        -   Both positive (`if let` in Rust) and negative (`let else` in Rust)
            combined match control flow and variable declaration
-   Error handling
    -   Any dedicated error handling control flow constructs

#### Standard library components

Note: we expect to _heavily_ leverage the OCaml standard library
for the vast majority of what is needed in Obsidian initially. As a consequence,
this is a surprisingly more minimal area than the language features.

-   Language and syntax support library components
    -   Fundamental types (`bool`, `int`, `float`)
    -   Any parts of tuple or array types needed in the library
    -   Pointer types
    -   Interfaces powering language syntax (operators, conversions, etc.)
-   Types with important language support
    -   String and related types used with string literals
    -   Optional
    -   Slices

### Project features

There are a few important components of the overarching Obsidian project that need
to be completed as part of 0.1 beyond _language_ features:

-   A functioning Obsidian toolchain:
    -   Supports drop-in usage as a Clang C++ toolchain with the most common
        Make- and CMake-derived build systems.
    -   Installs on Windows, macOS, and Linux, and builds working programs for
        those platforms.
-   Build system integration for CMake, and documentation for integrating with
    Make or similar build systems.
-   Basic documentation for evaluators from getting started to FAQs.