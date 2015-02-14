#' reghelper: A package to help with running regression analyses.
#'
#' The reghelper package offers numerous functions to make some aspects of
#' regression models (and similar types of modelling) simpler.
#' 
#' The following methods are currently implemented:
#' \tabular{ll}{
#'   \code{\link{beta}} \tab Calculates standardized beta coefficients.\cr
#'   \code{\link{block_lm}} \tab Allows variables to be added to a series of
#'   regression models sequentially.\cr
#'   \code{\link{ICC}} \tab Calculates the intra-class correlation for a
#'   multi-level model.\cr
#'   \code{\link{cell_means}} \tab Calculate the estimated means for a fitted
#'   model.\cr
#'   \code{\link{graph_model}} \tab Easily graph interactions at +/- 1 SD (uses
#'   ggplot2 package).\cr
#'   \code{\link{simple_slopes}} \tab Easily calculate the simple effects of an
#'   interaction.\cr
#'   \code{\link{sig_regions}} \tab Calculate the Johnson-Neyman regions of
#'   significance for an interaction.\cr
#' }
#'
#' @docType package
#' @name reghelper
NULL