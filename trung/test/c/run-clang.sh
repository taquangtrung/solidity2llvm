#!/bin/sh

input_file=$1
output_bc="${input_file%.*}"".bc"
output_ll="${input_file%.*}"".ll"

clang -g -O0 -emit-llvm -c $1 -o $output_bc
llvm-dis $output_bc
