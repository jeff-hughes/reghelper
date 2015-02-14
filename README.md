<!-- README.md is generated from README.Rmd. Please edit that file -->


reghelper
=========

The `reghelper` R package includes a set of functions used to automate commonly used methods in regression analysis. This includes plotting interactions, calculating simple slopes, calculating standardized coefficients, etc.

This package is still in the development stages, and is not ready for general use. Functions and function parameters may still be subject to change.

However, if you are interested in helping out with the development process, you can install the package in R with the following code:

``` {.R}
install.packages("devtools")
devtools::install_github("jeff-hughes/reghelper")
```

Current progress
----------------

So far, most functions that I had originally planned to include have been implemented for `lm` models. These functions include: \* `beta`: Calculates standardized beta coefficients. \* `block_lm`: Allows variables to be added to a series of regression models sequentially (similar to SPSS). \* `ICC` Calculates the intra-class correlation for a multi-level model (lme only at this point). \* `cell_means`: Calculates the estimated means for a fitted model. \* `graph_model`: Easily graph interactions at +/- 1 SD (uses ggplot2 package). \* `simple_slopes`: Easily calculate the simple effects of an interaction. \* `sig_regions`: Calculate the Johnson-Neyman regions of significance for an interaction.

Things to do
------------

There is still much work to be done. Below is a list of areas yet to be completed, so if you are interested in helping out, these may be good places to start: \* At minimum, I would like to extend these functions to `aov`, `glm`, `lme`, and `lmer` models. \* None of the documentation includes example code, which needs to be included. \* The package does include test cases, but more thorough testing needs to be done to ensure these functions work on a variety of models.
