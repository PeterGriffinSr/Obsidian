FROM ubuntu:latest

RUN apt-get update && \
    apt-get install -y \
    git \
    gnupg \
    wget \
    bubblewrap \
    curl \
    m4 \
    build-essential \
    pkg-config \
    lsb-release \
    software-properties-common \
    llvm-14 \
    clang \
    cmake \
    opam && \
    rm -rf /var/lib/apt/lists/*

RUN opam init -y --disable-sandboxing

RUN opam install dune menhir llvm.14.0.6 ppx_deriving odoc -y

WORKDIR /app

COPY . .

RUN eval $(opam env) && dune build
