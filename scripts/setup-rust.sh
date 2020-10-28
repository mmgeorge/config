#!/bin/bash

# Setup autocompletion
#rustup toolchain add nightly
#cargo +nightly install racer
#rustup component add rust-src

# Setup rust language server (RLS)
rustup update
rustup component add rls rust-analysis rust-src

