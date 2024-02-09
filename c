#!/bin/env sh

./repl p.scm > out.c
gcc out.c -o p

