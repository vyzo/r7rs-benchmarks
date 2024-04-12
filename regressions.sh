#!/bin/sh

./regression.ss results.Gerbil-sep results.Gerbil-sep-v18 > regression-safe-sep.html
./regression.ss results.Gerbil-unsafe-sep results.Gerbil-unsafe-sep-v18 > regression-unsafe-sep.html
./regression.ss results.Gerbil-unsafe-fpo results.Gerbil-unsafe-fpo-v18 > regression-unsafe-fpo.html
./regression.ss results.Gerbil-sep results.Gerbil-unsafe-sep > regression-safe-vs-unsafe.html
./regression.ss results.Gerbil-sep results.Racket > regression-gerbil-vs-racket.html
./regression.ss results.Gerbil-sep results.GambitC > regression-gerbil-vs-gambit.html
