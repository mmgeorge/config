#!/bin/bash

sudo apt install libxpm-dev libgif-dev libjpeg-dev libpng-dev libtiff-dev libx11-dev libncurses5-dev automake autoconf texinfo libgtk2.0-dev
sudo add-apt-repository ppa:ubuntu-toolchain-r/ppa
sudo apt install gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev
git clone git://git.sv.gnu.org/emacs.git
#git checkout feature/native-comp
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
./autogen.sh
./configure --with-nativecomp --with-json CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer"
make -j2 NATIVE_FULL_AOT=1
make install
