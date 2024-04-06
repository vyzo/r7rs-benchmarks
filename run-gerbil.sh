#!/bin/bash
for x in gerbil-shared gerbil-fpo gerbil-unsafe-shared gerbil-unsafe-fpo; do
    ./bench $x all
done
