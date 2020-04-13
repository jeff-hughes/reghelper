<!-- README.md is generated from README.Rmd. Please edit that file -->

# reghelper

The `reghelper` R package includes a set of functions used to automate
commonly used methods in regression analysis. This includes plotting
interactions, calculating simple slopes, calculating standardized
coefficients, etc.

Version 0.3.6 has been released. The most recent stable release is
available on CRAN, and can be installed like so:

``` r
install.packages("reghelper")
```

You can also install the stable release from Github:

``` r
install.packages("devtools")
devtools::install_github("jeff-hughes/reghelper@v0.3.6")
```

If you would like to install the latest development version, you can do
so with the following code:

``` r
install.packages("devtools")
devtools::install_github("jeff-hughes/reghelper")
```

### Installation Issues

Networked computers can sometimes result in installation issues, as the
`install_github` function sometimes has difficulty with networked
directories. If this happens to you, use the `.libPaths()` function to
find the path to your R libraries. That will likely give you a path
starting with two backslashes, but you will need to convert that to a
path starting with a drive letter (e.g., ‘C:’, ‘D:’). From there, use
the following code:

``` r
install.packages("devtools")
devtools::install_github("jeff-hughes/reghelper", args=c('--library="N:/path/to/libraries/"'))
```

Obviously, change the path to the path where your R libraries are
stored.

## Summary of helper functions

  - `beta` Calculates standardized beta coefficients.
  - `build_model` Allows variables to be added to a series of regression
    models sequentially (similar to SPSS).
  - `ICC` Calculates the intra-class correlation for a multi-level
    model.
  - `cell_means` Calculates the estimated means for a fitted model.
  - `graph_model` Easily graph interactions at +/- 1 SD (uses ggplot2
    package).
  - `sig_regions` Calculate the Johnson-Neyman regions of significance
    for an interaction.
  - `simple_slopes` Easily calculate the simple effects of an
    interaction.

The table below shows the current types of models for which each
function has been implemented:

| Function       | lm | glm | aov | lme | lmer |
| :------------- | :-: | :-: | :-: | :-: | :--: |
| beta           | ✓  |  ✓  |  ✓  |  –  |  –   |
| build\_model   | ✓  |  ✓  |  ✓  |     |      |
| ICC            | –  |  –  |  –  |  ✓  |  ✓   |
| cell\_means    | ✓  |  ✓  |  ✓  |     |      |
| graph\_model   | ✓  |  ✓  |  ✓  |  ✓  |  ✓   |
| sig\_regions   | ✓  |  ✓  |  –  |     |      |
| simple\_slopes | ✓  |  ✓  |  ✓  |  ✓  |  ✓   |

## Removal of beta() function for multilevel models (nlme and lme4)

Strategies for how to center Level 1 variables in multilevel models has
been a topic of discussion among methodologists, with a variety of
approaches suggested. Two of the most common approaches are grand-mean
centering vs. group-mean (or within-cluster) centering (Bryk &
Raudenbush, 1992; Enders & Tofighi, 2007). This latter approach could
actively be impaired by standardizing the variable after applying the
group-mean centering strategy, potentially leading to misinterpretation
of the model effects. Some researchers have suggested that Level 1
effects should be standardized within groups (i.e., using the group mean
and group standard deviation; Schuurman et al., 2016), but this is not a
universally accepted practice and strategies vary. Thus, there is no
agreed-upon method of standardizing the random effects of a model, and
applying one approach in an R function could have deleterious impacts on
interpretation.

Standardizing the fixed effects is less contentious, but there is no
restriction on what level variables can be found in the model, and in
some cases, variables may be included as both fixed and random effects.
Other complications in interpretation could arise if a model includes
cross-level effects (cf. Aguinis, Gottfredson, & Culpepper, 2013),
custom covariance structures, etc. Because of this, there is no real
“default” approach for which variables to standardize in a multilevel
model that will be suitable for all model structures, and the researcher
should give careful consideration to this based on the structure of
their particular model, and relevant theoretical considerations (Enders
& Tofighi, 2007; Nezlek, 2012).

Because of this, defining a `beta()` function in a consistent way for
multilevel models (nlme and lme4), in a way that would not lead to
potential misinterpretation and/or unexpected behaviour, proves
difficult. While the initial implementation of this function aimed to
standardize just the fixed effects and the outcome variable, this proved
insufficient—in cases where the same variable appears as a fixed effect
and random effect, what should happen to this variable? Surprising users
with models where some variables end up standardized and others are not
is not a suitable result.

As a consequence, the `beta()` function has been deprecated in v0.3.6
and will be removed in a future version. It will still remain
implemented for lm, glm, and aov models. Researchers who wish to obtain
standardized effects for their multilevel models should manually
standardize the variables that are appropriate given their model
structure and theory. However, consideration should be given to the
centering strategy used in their model as well. For further discussion
on these issues, please see the references below.

### References

Aguinis, H., Gottfredson, R. K., & Culpepper, S. A. (2013).
Best-practice recommendations for estimating cross-level interaction
effects using multilevel modeling. *Journal of Management, 39*(6),
1490-1528. <http://dx.doi.org/10.1177/0149206313478188>

Bryk, A. S., & Raudenbush, S. W. (1992). *Hierarchical linear models:
Applications and data analysis methods*. Sage Publications.

Enders, C. K., & Tofighi, D. (2007). Centering predictor variables in
cross-sectional multilevel models: A new look at an old issue.
*Psychological Methods, 12*(2), 121-138.
<http://dx.doi.org/10.1037/1082-989X.12.2.121>

Nezlek, J. B. (2012). Multilevel modeling for psychologists. In H.
Cooper (Ed.), *APA handbook of research methods in psychology* (Vol. 3).
(pp. 219-241). American Psychological Association.
<http://dx.doi.org/10.1037/13621-011>

Schuurman, N. K., Ferrer, E., de Boer-Sonnenschein, M., & Hamaker, E. L.
(2016). How to compare cross-lagged associations in a multilevel
autoregressive model. *Psychological Methods, 21*(2), 206–221.
<https://doi.org/10.1037/met0000062>
