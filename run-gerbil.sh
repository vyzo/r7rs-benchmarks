#!/bin/bash
for x in gerbil-sep gerbil-fpo gerbil-unsafe-sep gerbil-unsafe-sep; do
    ./bench $x all
done
