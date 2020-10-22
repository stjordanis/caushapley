Description
--------------

Shapley values underlie one of the most popular model-agnostic methods within 
explainable artificial intelligence. These values are designed to attribute the 
difference between a model’s prediction and an average baseline to the different 
features used as input to the model. Being based on solid game-theoretic principles, 
Shapley values uniquely satisfy several desirable properties, which is why they 
are increasinglyused to explain the predictions of possibly complex and highly 
non-linear machine learning models. Shapley values are well calibrated to a 
user’s intuition when features are independent, but may lead to undesirable, 
counterintuitive explanations when the independence assumption is violated.

In this paper, we propose a novel framework for computing Shapley values that
generalizes recent work that aims to circumvent the independence assumption.
By employing Pearl’s do-calculus, we show how these ‘causal’ Shapley values
can be derived for general causal graphs without sacrificing any of their desirable
properties. Moreover, causal Shapley values enable us to separate the contribution
of direct and indirect effects. We provide a practical implementation for computing
causal Shapley values based on causal chain graphs when only partial information
is available and illustrate their utility on a real-world example.

This R package represents the code supplement to the paper titled “Causal Shapley 
Values: Exploiting Causal Knowledge to Explain Individual Predictions of Complex Models”, 
authored by Tom Heskes, Evi Sijben, Ioan Gabriel Bucur and Tom Claassen, 
which has been published in the proceedings of NeurIPS 2020 (ID 10778).


Prerequisites
--------------
This code requires an [R](https://cran.r-project.org/) to be installed. On 
Windows, [Rtools](https://cran.r-project.org/bin/windows/Rtools/) is also required. 
After installing R (and Rtools), run the script *scripts/install.R* in the root directory 
to install the required R packages. These can also be found in the DESCRIPTION file.


Installation Instructions
--------------

1. Download the software from the RU GitLab with the following command:
`git clone --recurse-submodules https://gitlab.science.ru.nl/gbucur/CauSHAPley.git`.

2. Run `Rscript scripts/install.R` from the command line or `source("scripts/install.R")` 
if running R in the root directory.

WARNING: Running the script above will replace your current *shapr* installation, if there is one.


Package Structure
--------------
The code is structured on the skeleton of an [R package](https://r-pkgs.org/index.html) 
package as follows:

- The folder `R` contains R files for producing sina plots (`sina_plot.R`).

- The folder `man` contains the documentation for the implemented functions.

- The folder `shapr` contains the [shapr](https://gitlab.science.ru.nl/gbucur/shapr) submodule, 
which is a fork of the original [shapr](https://github.com/NorskRegnesentral/shapr) package by NorskRegnesentral.

- The folder `scripts` contains the script `bike_rental_illustration.R`, which 
can be run from R to reproduce the results from the paper on the bike sharing
example (Section 6).

- The subfolder `inst/extdata` contains data for the bike sharing example (`day.csv`) as
well as a list of training data indices (`train_index.rds`) which are used in the `scripts/bike_rental_illustration.R` for reproducibility purposes.

- The subfolder `inst/rmd` contains an R notebook showcasing an example of 
computing the Causal shapley values.

- The top folder also contains the following files:
  - `DESCRIPTION` is the file describing the R package
  - `NAMESPACE` is the file specifying the functions provided by the R package
  - `LICENSE.md` and `LICENSE` contain the MIT license.


Reproducing Results from Paper
--------------

To reproduce the bike rental example from the paper, run the script 
`bike_rental_illustration.R` located in the `scripts` subfolder:

`Rscript scripts/bike_rental_illustration.R` from the command line or `source("scripts/bike_rental_illustration.R")` if running R in the root directory.
By default the script will save the (reproduced) figures from the paper in the `figures` directory.


Acknowledgments
--------------

The code for this paper is an extension to the [shapr](https://github.com/NorskRegnesentral/shapr) package by NorskRegnesentral.

The code for the sina plot is adapted from the [SHAPforxgboost](https://cran.r-project.org/web/packages/SHAPforxgboost/index.html) package by Yang Liu.

The bike rental data has been publicly shared on the [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset) by Hadi Fanaee-T.
