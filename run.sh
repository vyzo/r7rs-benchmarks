#!/bin/bash
for x in racket gambitc gerbil-sep gerbil-fpo gerbil-unsafe-sep gerbil-unsafe-fpo; do
    ./bench $x all
done
