#!/bin/bash
for x in racket gambitc gerbil-shared gerbil-fpo gerbil-unsafe-shared gerbil-unsafe-fpo; do
    ./bench $x all
done
