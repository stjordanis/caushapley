---
title: "Code Supplement for 'Causal Shapley Values: Exploiting Causal Knowledge to Explain Individual Predictions of Complex Models' (NeurIPS ID 10778)"
---

==============

Installation Instructions
--------------

This code requires an [R installation](https://cran.r-project.org/). On Windows, an [Rtools installation](https://cran.r-project.org/bin/windows/Rtools/) is also required. After installing R (and Rtools), run the script *install.R* in the root directory to install the required packages:

`Rscript install.R` from the command line or `source("install.R")` if running R in the root directory.

WARNING: Running the script above will replace your current *shapr* installation, if there is one.

Reproducing Results from Paper
--------------

To reproduce the bike rental example from the paper, run the script *bike_rental_illustration.R* located in the *scripts* subfolder:

`Rscript scripts/bike_rental_illustration.R` from the command line or `source("scripts/bike_rental_illustration.R")` if running R in the root directory. By default the script will save the (reproduced) figures from the paper in the _figures_ directory.

Acknowledgments
--------------

The code for this paper is an extension to the [shapr](https://github.com/NorskRegnesentral/shapr) package by NorskRegnesentral.

The code for the sina plot is adapted from the [SHAPforxgboost](https://cran.r-project.org/web/packages/SHAPforxgboost/index.html) package by Yang Liu.

The bike rental data has been publicly shared on the [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset) by Hadi Fanaee-T.
