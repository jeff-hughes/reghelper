<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->


reghelper 0.3.0
===============

-   `graph_model` function extended to include `aov` and `glm` models.

-   `build_model` function extended to include `aov` and `glm` models.

-   `cell_means` function extended to include `glm` models.

reghelper 0.2.0
===============

MAJOR CHANGES

-   Changed `block_lm` function name to `build_model`.

NEW FEATURES

-   Added examples to documentation for all functions.

-   `beta` function extended to include `glm` models.

-   `cell_means` function extended to include `aov` models.

-   `ICC` function extended to include `merMod` models (from "lme4" package).

-   `simple_slopes` function extended to include `aov`, `glm`, `lme`, and `merMod` models.

-   `simple_slopes` now includes `print` function to include significance stars.

BUG FIXES

-   Fixed bug with passing variables names to `build_model`, `cell_means`, and `graph_model`. Resolves Issue \#1.

reghelper 0.1.0
===============

NEW FEATURES

-   `beta` function calculates standardized beta coefficients.

-   `block_lm` function allows variables to be added to a series of regression models sequentially (similar to SPSS).

-   `ICC` function calculates the intra-class correlation for a multi-level model (lme only at this point).

-   `cell_means` function calculates the estimated means for a fitted model.

-   `graph_model` function graphs interactions at +/- 1 SD (uses ggplot2 package).

-   `simple_slopes` function calculates the simple effects of an interaction.

-   `sig_regions` function calculate the Johnson-Neyman regions of significance for an interaction.
