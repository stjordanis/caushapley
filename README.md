---
title: "Code Supplement for 'Causal Shapley Values: Exploiting Causal Knowledge to Explain Individual Predictions of Complex Models' (NeurIPS ID 10778)"
---

==============

Installation Instructions
--------------

This code requires an [R installation](https://cran.r-project.org/). After installing R, run the script *build.R* in the root directory to install the required packages:

`R build.R`.

WARNING: The code is based on the [*shapr*](https://github.com/NorskRegnesentral/shapr) package. If this package is already in your library, running the build script will replace the current package installation.

Reproducing Results from Paper
--------------

To reproduce the bike rental example from the paper, run the script *bike_rental_illustration.R* located in the *scripts* subfolder:

`R scripts/bike_rental_illustration.R`.

By default the script will save the (reproduced) figures from the paper. These will be saved in the root directory.