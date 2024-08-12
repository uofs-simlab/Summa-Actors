#! /bin/bash

# Create directory to create symlinks for correct compiler and linker
if [ ! -d "~/.local/bin" ]; then
    mkdir -p ~/.local/bin
fi

# Create simlinks
if [ ! -f "~/.local/bin/ld" ]; then
    ln -s /usr/bin/ld ~/.local/bin/ld
fi

if [ ! -f "~/.local/bin/gcc" ]; then
    ln -s /usr/bin/gcc/ ~/.local/bin/gcc
fi


export PATH=~/.local/bin:$PATH

make -f Makefile

