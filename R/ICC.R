#' Intra-class correlation.
#' 
#' \code{ICC} calculates the intra-class correlation (ICC) from a fitted
#' hierarchical linear model using the 'nlme' or 'lme4' packages.
#' 
#' The ICC is the proportion of variance that is between-person variance. For
#' more information, see
#' \href{http://davidakenny.net/papers/k&h/MLM_R.pdf}{Hoyt & Kenny (2013)}.
#' 
#' @param model A fitted linear model of type 'lme' (nlme) or 'merMod' (lme4;
#'   linear, generalized, or nonlinear).
#' @param ... Not currently implemented; used to ensure consistency with S3 generic.
#' @return The form of the value returned by \code{ICC} depends on the class of
#'   its argument. See the documentation of the particular methods for details
#'   of what is produced by that method.
#' @examples
#' # iris data, showing use with lme()
#' if (require(nlme, quietly=TRUE)) {
#'     model <- lme(Sepal.Width ~ 1, random=~1|Species, data=iris)
#'     ICC(model)  # .49 of variance is between-subjects
#' }
#' 
#' # iris data, showing use with lmer()
#' if (require(lme4, quietly=TRUE)) {
#'     model <- lmer(Sepal.Width ~ 1 + (1|Species), data=iris)
#'     ICC(model)  # .49 of variance is between-subjects
#' }
#' @export
ICC <- function(model, ...) UseMethod('ICC')


#' @describeIn ICC Intra-class correlation for lme (nlme).
#' @export
ICC.lme <- function(model, ...) {
    variance <- nlme::VarCorr(model)
    var_between <- as.numeric(variance[1:(nrow(variance)-1)])
    var_total <- as.numeric(variance[1:nrow(variance)])
    return(sum(var_between)/sum(var_total))
}


#' @describeIn ICC Intra-class correlation for lmer (lme4).
#' @export
ICC.merMod <- function(model, ...) {
    variance <- as.data.frame(lme4::VarCorr(model))
    var_total <- variance[is.na(variance$var2), 'vcov']
    var_between <- var_total[1:(length(var_total)-1)]
    return(sum(var_between)/sum(var_total))
}




