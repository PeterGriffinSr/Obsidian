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
    opam

RUN opam init -y --disable-sandboxing

RUN opam install dune menhir llvm.14.0.6 ppx_deriving odoc -y

COPY . .
WORKDIR /app

RUN dune build
RUN ./_build/default/bin/obsidian.exe examples/hello.ob

CMD [ "./a.out" ]