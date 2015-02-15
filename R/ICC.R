#' Intra-class correlation.
#' 
#' \code{ICC} is a generic function for calculating the intra-class correlation
#' (ICC) for a fitted model.
#' 
#' @param model A fitted linear model of type 'lme'.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given model.
#' @return The form of the value returned by \code{ICC} depends on the class of
#'   its argument. See the documentation of the particular methods for details
#'   of what is produced by that method.
#' @seealso \code{\link{sig_regions.lm}}
#' @examples TODO: Need to complete.
#' @export
ICC <- function(model, ...) UseMethod('ICC')


#' Intra-class correlation.
#' 
#' \code{ICC.lme} calculates the intra-class correlation (ICC) from a fitted
#' multi-level model using the 'nlme' package.
#' 
#' The ICC is the proportion of variance that is between-person variance. For
#' more information, see
#' \href{http://davidakenny.net/papers/k&h/MLM_R.pdf}{Hoyt & Kenny (2013)}.
#' 
#' @param model A fitted model of type 'lme'.
#' @return The intra-class correlation of the model.
#' @examples TODO: Need to complete.
#' @export
ICC.lme <- function(model) {
    variance <- VarCorr(model)
    var_between <- as.numeric(variance[1:(nrow(variance)-1)])
    var_total <- as.numeric(variance[1:nrow(variance)])
    return(sum(var_between)/sum(var_total))
}




