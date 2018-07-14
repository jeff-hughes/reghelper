#' Summary functions for build_model block regression models.
#' 
#' These functions offer useful methods for objects created by the
#' \code{build_model} function: \code{block_lm}, \code{block_aov}, and
#' \code{block_glm}.
#' 
#' @param object An object of class \code{block_lm}, \code{block_aov}, or
#'   \code{block_glm}, usually, a result of a call to \code{\link{build_model}}.
#' @param x An object of class \code{block_lm_summary}, \code{block_aov_summary},
#'   or \code{block_glm_summary}, usually, a result of a call to
#'   the corresponding \code{summary} function (e.g., \code{summary.block_lm}).
#' @param digits The number of significant digits to use when printing.
#' @param signif.stars Logical. If \code{TRUE}, 'significance stars' are printed
#'   for each coefficient.
#' @param num Numeric vector with the index of model(s) from which to return the
#'   requested output. If NULL, will return output from all blocks.
#' @param ... Further arguments passed to or from other methods.
#' @return The \code{summary} function computes and returns a named list of
#'   summary statistics of the fitted linear models given in \code{object}. The
#'   list has the following elements:
#' \tabular{ll}{
#'   \code{formulas} \tab A list of the regression formulas used for each block.
#'   \cr
#'   \code{residuals} \tab A matrix with quantiles of the residuals for each
#'   model.\cr
#'   \code{coefficients} \tab A list with a matrix of coefficients for each
#'   model, as well as the standard error, t-statistic, and p-value.\cr
#'   \code{overall} \tab A data frame with information about the overall models,
#'   including the multiple R-squared value; adjusted R-squared; F-statistic,
#'   degrees of freedom, and p-value for each overall model; and the delta
#'   R-squared (change in R-squared) and its associated F-statistic and p-value.
#'   \cr
#' }
#'   The other functions listed here provide convenient access to the individual
#'   components of this summary.
#' @examples
#' # 2 blocks: Petal.Length; Petal.Length + Petal.Width
#' model1 <- build_model(Sepal.Length, Petal.Length, Petal.Width, data=iris, model='lm')
#' summary(model1)
#' coef(model1)  # returns coefficients from both blocks 1 and 2
#' 
#' # 2 blocks: Species; Species + Petal.Length + Petal.Width + Petal.Length:Petal.Width
#' model2 <- build_model(Sepal.Length, Species, c(Petal.Length * Petal.Width), data=iris, model='lm')
#' summary(model2)
#' coef(model2, num=2)  # returns coefficients from second block only
#' @name block_model_summ
NULL