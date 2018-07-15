<!-- README.md is generated from README.Rmd. Please edit that file -->
reghelper
=========

The `reghelper` R package includes a set of functions used to automate commonly used methods in regression analysis. This includes plotting interactions, calculating simple slopes, calculating standardized coefficients, etc.

Version 0.3.4 has been released. However, be aware that this package is still in development, and as such, bugs may still exist, and functions and function parameters may still be subject to change.

The most recent stable release is available on CRAN, and can be installed like so:

``` r
install.packages("reghelper")
```

You can also install the stable release from Github:

``` r
install.packages("devtools")
devtools::install_github("jeff-hughes/reghelper")
```

If you would like to install the latest development version, you can do so with the following code:

``` r
install.packages("devtools")
devtools::install_github("jeff-hughes/reghelper@develop")
```

### Installation Issues

Networked computers can sometimes result in installation issues, as the `install_github` function sometimes has difficulty with networked directories. If this happens to you, use the `.libPaths()` function to find the path to your R libraries. That will likely give you a path starting with two backslashes, but you will need to convert that to a path starting with a drive letter (e.g., 'C:', 'D:'). From there, use the following code:

``` r
install.packages("devtools")
devtools::install_github("jeff-hughes/reghelper", args=c('--library="N:/path/to/libraries/"'))
```

Obviously, change the path to the path where your R libraries are stored.

Current progress
----------------

So far, most functions that I had originally planned to include have been implemented for `lm` models. These functions include:

-   `beta` Calculates standardized beta coefficients.
-   `build_model` Allows variables to be added to a series of regression models sequentially (similar to SPSS).
-   `ICC` Calculates the intra-class correlation for a multi-level model.
-   `cell_means` Calculates the estimated means for a fitted model.
-   `graph_model` Easily graph interactions at +/- 1 SD (uses ggplot2 package).
-   `sig_regions` Calculate the Johnson-Neyman regions of significance for an interaction.
-   `simple_slopes` Easily calculate the simple effects of an interaction.

The table below shows the current types of models for which each function has been implemented:

| Function       |  lm | glm | aov | lme | lmer |
|:---------------|:---:|:---:|:---:|:---:|:----:|
| beta           |  ✓  |  ✓  |  ✓  |  ✓  |   ✓  |
| build\_model   |  ✓  |  ✓  |  ✓  |     |      |
| ICC            |  –  |  –  |  –  |  ✓  |   ✓  |
| cell\_means    |  ✓  |  ✓  |  ✓  |     |      |
| graph\_model   |  ✓  |  ✓  |  ✓  |  ✓  |   ✓  |
| sig\_regions   |  ✓  |  ✓  |  –  |     |      |
| simple\_slopes |  ✓  |  ✓  |  ✓  |  ✓  |   ✓  |
