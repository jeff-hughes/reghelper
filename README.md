<!-- README.md is generated from README.Rmd. Please edit that file -->
reghelper
=========

The `reghelper` R package includes a set of functions used to automate commonly used methods in regression analysis. This includes plotting interactions, calculating simple slopes, calculating standardized coefficients, etc.

Version 0.3.0 has been released. However, be aware that this package is still in development, and as such, bugs may still exist, and functions and function parameters may still be subject to change.

To install the most recent stable release, use the following code:

``` r
install.packages("devtools")
devtools::install_github("jeff-hughes/reghelper")
```

If you are interested in helping out with the development process, you can install the development branch with the following code:

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

<table style="width:82%;">
<colgroup>
<col width="19%" />
<col width="12%" />
<col width="12%" />
<col width="12%" />
<col width="12%" />
<col width="12%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Function</th>
<th align="center">lm</th>
<th align="center">glm</th>
<th align="center">aov</th>
<th align="center">lme</th>
<th align="center">lmer</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">beta</td>
<td align="center">✓</td>
<td align="center">✓</td>
<td align="center">✓</td>
<td align="center">✓</td>
<td align="center">✓</td>
</tr>
<tr class="even">
<td align="left">build_model</td>
<td align="center">✓</td>
<td align="center">✓</td>
<td align="center">✓</td>
<td align="center"></td>
<td align="center"></td>
</tr>
<tr class="odd">
<td align="left">ICC</td>
<td align="center">–</td>
<td align="center">–</td>
<td align="center">–</td>
<td align="center">✓</td>
<td align="center">✓</td>
</tr>
<tr class="even">
<td align="left">cell_means</td>
<td align="center">✓</td>
<td align="center">✓</td>
<td align="center">✓</td>
<td align="center"></td>
<td align="center"></td>
</tr>
<tr class="odd">
<td align="left">graph_model</td>
<td align="center">✓</td>
<td align="center">✓</td>
<td align="center">✓</td>
<td align="center">✓</td>
<td align="center">✓</td>
</tr>
<tr class="even">
<td align="left">sig_regions</td>
<td align="center">✓</td>
<td align="center">✓</td>
<td align="center">–</td>
<td align="center"></td>
<td align="center"></td>
</tr>
<tr class="odd">
<td align="left">simple_slopes</td>
<td align="center">✓</td>
<td align="center">✓</td>
<td align="center">✓</td>
<td align="center">✓</td>
<td align="center">✓</td>
</tr>
</tbody>
</table>

Things to do
------------

There is still much work to be done. Below is a list of areas yet to be completed, so if you are interested in helping out, these may be good places to start:

-   At minimum, I would like to extend these functions to `aov`, `glm`, `lme`, and `lmer` models.
-   The package does include test cases, but more thorough testing needs to be done to ensure these functions work on a variety of models.
