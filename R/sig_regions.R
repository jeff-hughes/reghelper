#' Regions of significance for an interaction.
#' 
#' \code{sig_regions} is a generic function for calculating the Johnson-Neyman
#' regions of significance for an interaction, the points at which the simple
#' effect of the categorical predictor changes from non-significant to
#' significant.
#' 
#' @param model A fitted linear model of type 'lm'.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given model.
#' @return The form of the value returned by \code{sig_regions} depends on the
#'   class of its argument. See the documentation of the particular methods for
#'   details of what is produced by that method.
#' @seealso \code{\link{sig_regions.lm}}, \code{\link{simple_slopes}}
#' @examples
#' # mtcars data
#' mtcars$am <- factor(mtcars$am)  # make 'am' categorical
#' model <- lm(mpg ~ wt * am, data=mtcars)
#' summary(model)  # significant interaction
#' sig_regions(model)
#' @export
sig_regions <- function(model, ...) UseMethod('sig_regions')


#' Regions of significance for an interaction.
#' 
#' \code{sig_regions.lm} calculates the Johnson-Neyman (J-N) regions of
#' significance for an interaction, the points at which the simple effect of the
#' categorical predictor changes from non-significant to significant.
#' 
#' This function takes a regression model with one two-way interaction, where
#' one of the predictors in the interaction is categorical (factor) and the
#' other is continuous. For other types of interaction terms, use the
#' \code{\link{simple_slopes}} function instead.
#' 
#' For more information about regions of significance, see
#' \href{http://ssrn.com/abstract=2208103}{Spiller, Fitzsimons, Lynch, &
#' McClelland (2012)}.
#' 
#' @param model A fitted linear model of type 'lm' with one two-way interaction
#'   including one categorical predictor and one continuous variable.
#' @param alpha The level at which to test for significance. Default value is
#'   .05.
#' @param precision The number of decimal places to which to round the alpha
#'   level (e.g., precision=5 would look for regions of significance at .05000).
#' @param ... Not currently implemented; used to ensure consistency with S3 generic.
#' @return A named vector with a 'lower' and an 'upper' J-N point. If one or
#'   more of the J-N points fall outside the range of your predictor, the
#'   function will return NA for that point. If your interaction is not
#'   significant, both J-N points will be NA.
#' @seealso \code{\link{simple_slopes.lm}}
#' @examples
#' # mtcars data
#' mtcars$am <- factor(mtcars$am)  # make 'am' categorical
#' model <- lm(mpg ~ wt * am, data=mtcars)
#' summary(model)  # significant interaction
#' sig_regions(model)
#' @export
sig_regions.lm <- function(model, alpha=.05, precision=4, ...) {
    int_term <- which(attr(terms(model), 'order') == 2)
        # get location of interaction term
    
    # if more than one 2-way interaction, pick first one
    if (length(int_term) > 1) {
        int_term <- int_term[1]
    }
    
    int_vars <- which(attr(terms(model), 'factors')[, int_term] == 1)
        # get location of variables in the interaction
    factor_var <- int_vars == which(attr(terms(model), 'dataClasses') == 'factor')
        # indicates TRUE for the variable in int_vars that is a factor
    
    # check that interaction contains one categorical variable and one
    # continuous variable
    if (sum(factor_var) > 1) {
        stop('Interaction contains more than one factor variable.')
    }
    if (sum(factor_var) == 0) {
        stop('Interaction has no factor variables.')
    }
    
    cont_var_name <- names(int_vars[which(!factor_var)])
    
    coef <- coef(model)
    intersect <- -coef[[int_vars[which(factor_var)]]] / coef[[int_term+1]]
        # find point at which the interaction lines intersect each other
    
    min_x <- min(model$model[cont_var_name], na.rm=TRUE)
    max_x <- max(model$model[cont_var_name], na.rm=TRUE)
    
    points <- c(lower=NA, upper=NA)
    
    # disordinal interaction -- find both points
    if (intersect >= min_x && intersect <= max_x) {
        points['lower'] <- .search_sequence(
            model,
            int_vars,
            start=min_x,
            end=intersect,
            alpha=alpha,
            precision=precision)
        points['upper'] <- .search_sequence(
            model,
            int_vars,
            start=intersect,
            end=max_x,
            alpha=alpha,
            precision=precision)
        
    # intersect falls past lower end of scale
    } else if (intersect < min_x) {
        points['upper'] <- .search_sequence(
            model,
            int_vars,
            start=min_x,
            end=max_x,
            alpha=alpha,
            precision=precision)
        
    # intersect falls past upper end of scale
    } else if (intersect > max_x) {
        points['lower'] <- .search_sequence(
            model,
            int_vars,
            start=min_x,
            end=max_x,
            alpha=alpha,
            precision=precision)
    }
    
    return(points)
}


#' @rdname sig_regions.lm
#' @export
sig_regions.glm <- function(model, alpha=.05, precision=4, ...) {
    sig_regions.lm(model, alpha, precision)
}


#' Test where simple effects become significant.
#' 
#' Helper function recursively searches through sequences of predictor values to
#' search for points at which the p-value changes from non-significant to
#' significant.
#' 
#' @param model A fitted linear model of type 'lm' with one two-way interaction
#'   including at least one categorical predictor.
#' @param int_vars A vector listing the names of the variables in the
#'   interaction.
#' @param start The first value of the predictor to test. A NULL value will set
#'   the start value to the minimum value of the predictor.
#' @param end The last value of the predictor to test. A NULL value will set the
#'   end value to the maximum value of the predictor.
#' @param alpha The level at which to test for significance. Default value is
#'   .05.
#' @param precision The number of decimal places to which to round the alpha
#'   level (e.g., precision=5 would look for regions of significance at .05000).
#' @return A predictor value in the sequence, if any, for which the p-value is
#'   at the alpha level, rounded to the requested value of precision.
.search_sequence <- function(model, int_vars, start=NULL, end=NULL,
                             alpha=.05, precision=4) {
    
    mdata <- model$model
    factor_var <- int_vars == which(attr(terms(model), 'dataClasses') == 'factor')
    factor_var_name <- names(int_vars[which(factor_var)])
    cont_var_name <- names(int_vars[which(!factor_var)])
    
    factor_contrasts <- contrasts(model$model[, factor_var_name])
    if (is.null(colnames(factor_contrasts))) {
        dummy_name <- paste0(factor_var_name, '1')
    } else {
        dummy_name <- paste0(factor_var_name,
            colnames(factor_contrasts)[1])
    }
    
    if (is.null(start)) {
        start <- min(model$model[cont_var_name], na.rm=TRUE)
    }
    if (is.null(end)) {
        end <- max(model$model[cont_var_name], na.rm=TRUE)
    }    
    
    call <- model$call
    form <- format(formula(model))
    
    sequence <- seq(start, end, length.out=10)
    pvalues <- c()
    counter <- 1
    
    # cycle through sequence and pull out p values
    for (i in sequence) {
        # create new formula that shifts 0 point of continuous variable
        new_var_name <- paste0('I(', cont_var_name, ' - ', i, ')')
        new_form <- gsub(cont_var_name, new_var_name, form)
        
        call[['formula']] <- new_form
        call[['data']] <- quote(mdata)
        new_model <- eval(call)
        pvalues[counter] <- coef(summary(new_model))[dummy_name, 4]
            # pull out p-value for factor variable
        counter <- counter + 1
    }
    
    xvalue <- NA
    # if we find a value that is exactly the alpha level, return
    if (any(round(pvalues, precision) == alpha)) {
        xvalue <- sequence[min(which(round(pvalues, precision) == alpha))]
        
    # otherwise, look for pairs of values for which the p value is above
    # the alpha level for one and below the alpha level for the other
    } else {
        match <- 0
        for(j in 1:(length(pvalues)-1)) {
            if ((pvalues[j] > alpha && pvalues[j+1] < alpha) ||
                (pvalues[j] < alpha && pvalues[j+1] > alpha)) {
                
                match <- 1
                xvalue <- Recall(model, int_vars, sequence[j], sequence[j+1],
                    alpha=alpha, precision=precision)
                    # recurse
            }
        }
    }
    return(xvalue)
}



