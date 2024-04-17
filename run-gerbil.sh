#!/bin/bash
for x in gerbil-sep gerbil-unsafe-sep gerbil-typed; do
    ./bench $x all
done
