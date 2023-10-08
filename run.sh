#!/bin/bash
for x in racket gerbil gerbil-unsafe-sep gerbil-unsafe; do
    ./bench $x all
done
