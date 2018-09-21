---
layout: post
---
# LuaJIT and SBCL Benchmark

## Overview

This is a quick benchmark comparing [LuaJit](http://luajit.org) and [SBCL](http://sbcl.org) using the Mandelbrot set
program from [Debian's benchmark site.](https://benchmarksgame-team.pages.debian.net/benchmarksgame/)

## Software Versions

For this comparison I built both compilers from source using the tip of their Git repos as of this past Tuesday night.
For SBCL I'm using commit 92951c6f4 and for LuaJIT I'm using b025b01c5b.

I'm not familiar with the LuaJIT build process, so I used the default build procedure, and ran "make" and then "sudo
make install".

For SBCL I built with the following features turned on: (:sb-show-assem :immobile-space :compact-instance-header
:sb-thread :sb-futex :sb-xref-for-internals).  I compiled and installed with "make.sh --dynamic-space-size=8192" and
then "sudo sh install.sh"

```bash
$> sbcl --version
SBCL 1.4.11.92-92951c6f4
$>
```


```bash
$> luajit -v
LuaJIT 2.0.5 -- Copyright (C) 2005-2017 Mike Pall. http://luajit.org/
$>
```

## Programs

For the LuaJIT program I'm using the fastest Lua code from the benchmark site with no changes.  The code can be found [here](https://gist.github.com/jl2/6e42e3c91c0ec87a1a0537c7b2827d96#file-mandelbrot-lua).

The fastest Lisp code uses SBCL's assembly intrinsics, so I used the second fastest program.  I modified it to take a
thread count argument and also tweaked the build flags to create an executable instead of a core.  The code can be found [here](https://gist.github.com/jl2/6e42e3c91c0ec87a1a0537c7b2827d96#file-mandelbrot-lisp).

## Test Procedure

I created two shell scripts ([run_lua.sh](https://gist.github.com/jl2/6e42e3c91c0ec87a1a0537c7b2827d96#file-run_lua-sh) and [run_lisp.sh](https://gist.github.com/jl2/6e42e3c91c0ec87a1a0537c7b2827d96#file-run_lisp-sh)) to run the benchmark.  Images of size 10000 and 20000 were
generated using 4 and 8 threads, and each run was repeated 8 times.

| # Runs |  Size | Threads |
| ------ | ----- | ------- |
|      8 | 10000 |       4 |
|      8 | 20000 |       4 |
|      8 | 10000 |       8 |
|      8 | 20000 |       8 |

## LuaJIT Results

### Size 10000, 4 Threads

| User   | System | % CPU | Total |
| ------ | ------ | ----- | ----- |
| 14.13s | 0.16s  |  283% | 5.034 |
| 14.06s | 0.12s  |  308% | 4.591 |
| 14.11s | 0.13s  |  291% | 4.883 |
| 14.13s | 0.14s  |  281% | 5.073 |
| 14.14s | 0.12s  |  310% | 4.596 |
| 13.96s | 0.12s  |  279% | 5.031 |
| 14.10s | 0.13s  |  282% | 5.043 |
| 14.09s | 0.12s  |  280% | 5.065 |

### Size 10000, 8 Threads

| User   | System |  %CPU | Total |
| ------ | ------ | ----- | ----- |
| 14.83s | 0.11s  |  378% | 3.944 |
| 14.62s | 0.13s  |  357% | 4.122 |
| 14.67s | 0.14s  |  361% | 4.097 |
| 14.60s | 0.13s  |  355% | 4.141 |
| 14.57s | 0.10s  |  357% | 4.111 |
| 14.70s | 0.15s  |  365% | 4.064 |
| 14.71s | 0.16s  |  366% | 4.058 |
| 14.82s | 0.11s  |  376% | 3.966 |

### Size 20000, 4 Threads

| User   | System | % CPU |  Total |
| ------ | ------ | ----- | ------ |
| 59.50s | 0.45s  |  283% | 21.166 |
| 59.53s | 0.43s  |  285% | 20.992 |
| 59.93s | 0.43s  |  281% | 21.478 |
| 59.35s | 0.42s  |  290% | 20.543 |
| 59.21s | 0.41s  |  283% | 20.997 |
| 59.28s | 0.44s  |  303% | 19.695 |
| 59.50s | 0.45s  |  291% | 20.545 |
| 59.62s | 0.42s  |  295% | 20.329 |

### Size 20000, 8 Threads

| User   | System | % CPU |  Total |
| ------ | ------ | ----- | ------ |
| 61.61s | 0.54s  |  376% | 16.514 |
| 61.83s | 0.46s  |  376% | 16.537 |
| 61.82s | 0.47s  |  375% | 16.584 |
| 61.52s | 0.47s  |  377% | 16.433 |
| 61.76s | 0.44s  |  373% | 16.635 |
| 61.66s | 0.48s  |  371% | 16.722 |
| 61.52s | 0.43s  |  370% | 16.738 |
| 61.86s | 0.47s  |  371% | 16.763 |

## SBCL Results

### Size 10000, 4 Threads

| User   | System | % CPU | Total |
| ------ | ------ | ----- | ----- |
| 13.32s | 0.03s  |  278% | 4.801 |
| 13.34s | 0.02s  |  276% | 4.833 |
| 13.30s | 0.03s  |  275% | 4.831 |
| 13.47s | 0.02s  |  275% | 4.903 |
| 13.31s | 0.03s  |  276% | 4.832 |
| 13.32s | 0.01s  |  275% | 4.831 |
| 13.31s | 0.02s  |  276% | 4.827 |
| 13.32s | 0.02s  |  275% | 4.833 |

### Size 10000, 8 Threads

| User   | System | % CPU | Total |
| ------ | ------ | ----- | ----- |
| 14.73s | 0.03s  |  358% | 4.123 |
| 14.72s | 0.04s  |  357% | 4.133 |
| 14.72s | 0.03s  |  354% | 4.158 |
| 14.74s | 0.04s  |  357% | 4.129 |
| 14.77s | 0.03s  |  354% | 4.170 |
| 14.67s | 0.02s  |  353% | 4.163 |
| 14.72s | 0.03s  |  356% | 4.136 |
| 14.70s | 0.05s  |  354% | 4.163 |

### Size 20000, 4 Threads

| User   | System | % CPU |  Total |
| ------ | ------ | ----- | ------ |
| 53.14s | 0.07s  |  276% | 19.271 |
| 54.11s | 0.09s  |  274% | 19.780 |
| 53.10s | 0.08s  |  276% | 19.266 |
| 53.11s | 0.10s  |  275% | 19.335 |
| 53.00s | 0.06s  |  274% | 19.364 |
| 53.11s | 0.09s  |  275% | 19.291 |
| 54.57s | 0.07s  |  272% | 20.066 |
| 53.13s | 0.08s  |  275% | 19.283 |

### Size 20000, 8 Threads

| User   | System | % CPU |  Total |
| ------ | ------ | ----- | ------ |
| 58.92s | 0.07s  |  367% | 16.040 |
| 58.67s | 0.08s  |  354% | 16.557 |
| 58.68s | 0.07s  |  355% | 16.513 |
| 58.78s | 0.07s  |  356% | 16.506 |
| 58.53s | 0.08s  |  349% | 16.765 |
| 58.69s | 0.09s  |  352% | 16.668 |
| 58.59s | 0.08s  |  352% | 16.636 |
| 58.72s | 0.09s  |  355% | 16.530 |
