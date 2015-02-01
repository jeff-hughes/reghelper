#' Standardized coeffients of a model.
#' 
#' \code{beta} is a generic function for producing standardized coefficients
#' from regression models.
#' 
#' @param model A fitted linear model of type 'lm' or 'aov'.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given model.
#' @return The form of the value returned by \code{beta} depends on the class of
#'   its argument. See the documentation of the particular methods for details
#'   of what is produced by that method.
#' @seealso \code{\link{beta.lm}}, \code{\link{beta.aov}}
#' @examples TODO: Need to complete.
#' @export
beta <- function(model, ...) UseMethod('beta')


#' Standardized coeffients of a model.
#' 
#' \code{beta.lm} returns the summary of a linear model where all variables have
#' been standardized.
#' 
#' This function takes a linear regression model and standardizes the variables,
#' in order to produce standardized (i.e., beta) coefficients rather than
#' unstandardized (i.e., B) coefficients.
#' 
#' Unlike similar functions, this function properly calculates standardized
#' estimates for interaciton terms (by first standardizing each of the
#' individual predictor variables).
#' 
#' @param model A fitted linear model of type 'lm'.
#' @param x Logical. Whether or not to standardize predictor variables.
#' @param y Logical. Whether or not to standardize criterion variables.
#' @param skip A string vector indicating any variables you do \emph{not} wish
#'   to be standarized.
#' @return Returns the summary of a linear model, with the output showing the
#'   beta coefficients, standard error, t-values, and p-values for each
#'   predictor.
#' @examples TODO: Need to complete.
#' @export
beta.lm <- function(model, x=TRUE, y=TRUE, skip=NULL) {
    data <- model$model
    vars <- names(data)
    data_classes <- attr(terms(model), 'dataClasses')
    formula <- format(formula(model))
    lhs <- attr(terms(model), 'response')  # index of criterion variable(s)
    if (x == FALSE) {
        vars <- vars[lhs]  # get only variables on left-hand side
        data_classes <- data_classes[lhs]
    }
    if (y == FALSE) {
        vars <- vars[-lhs]  # get only variables on right-hand side
        data_classes <- data_classes[-lhs]
    }
    
    for (i in 1:length(vars)) {
        if (!(vars[i] %in% skip)) {
            
            # handle factor variables specially
            if (data_classes[i] == 'factor') {
                contrasts <- contrasts(data[, vars[i]])
                var_replace <- ''
                
                # need to break apart contrasts of factors, creating each dummy
                # variable separately, in order to scale them
                for (j in 1:ncol(contrasts)) {
                    # name each dummy code
                    if (is.null(colnames(contrasts))) {
                        var_name <- paste0(vars[i], j, '.z')
                    } else {
                        var_name <- paste0(vars[i], colnames(contrasts)[j], '.z')
                    }
                    if (ncol(contrasts) > 1) {
                        var_replace <- paste0(var_replace, ' + ', var_name)
                    } else {
                        var_replace <- var_name
                    }
                    
                    # add dummy code to data frame
                    data[, var_name] <- data[, vars[i]]
                    levels(data[, var_name]) <- contrasts[, j]
                    data[, var_name] <- as.numeric(
                        as.character(data[, var_name]))
                        # unfactor the dummy code
                    data[, var_name] <- scale(data[, var_name])
                }
                
                if (ncol(contrasts) > 1) {
                    var_replace <- paste0('(', var_replace, ')')
                        # add individual dummy codes to formula
                }
                formula <- gsub(vars[i], var_replace, formula)
                
            # all other variables
            } else {
                var_name <- paste0(vars[i], '.z')
                formula <- gsub(vars[i], var_name, formula)
                data[, var_name] <- scale(data[, vars[i]])
                    # add scaled variable to data frame
            }
        }
    }
    summary(lm(formula, data))
}

#' Standardized coeffients of a model.
#' 
#' \code{beta.aov} is an alias of beta.lm.
#' 
#' @seealso \code{\link{beta.lm}}
#' @export
beta.aov <- function(model, x=TRUE, y=TRUE, skip=NULL) {
    beta.lm(model, x, y, skip)
}



