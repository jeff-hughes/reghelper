#' Standardized coefficients of a model.
#' 
#' \code{beta} returns the summary of a linear model where all variables have
#' been standardized. It takes a regression model and standardizes the variables,
#' in order to produce standardized (i.e., beta) coefficients rather than
#' unstandardized (i.e., B) coefficients.
#' 
#' Unlike similar functions, this function properly calculates standardized
#' estimates for interaction terms (by first standardizing each of the predictor
#' variables separately, rather than using the standard deviation of the
#' interaction term itself).
#' 
#' @param model A fitted linear model of type 'lm', 'glm', 'aov', 'lme', or
#'   'merMod'.
#' @param x Logical. Whether or not to standardize predictor variables.
#' @param y Logical. Whether or not to standardize criterion variables.
#' @param skip A string vector indicating any variables you do \emph{not} wish
#'   to be standardized.
#' @param ... Not currently implemented; used to ensure consistency with S3 generic.
#' @return Returns the summary of a regression model, with the output showing 
#'   the standardized coefficients, standard error, t-values, and p-values for
#'   each predictor. The exact form of the values returned depends on the class
#'   of regression model used.
#' @examples
#' # iris data, showing use with lm()
#' model1 <- lm(Sepal.Length ~ Petal.Length + Petal.Width, iris)
#' beta(model1)  # all three variables standardized
#' 
#' model2 <- lm(Sepal.Width ~ Petal.Width + Species, iris)
#' beta(model2, skip='Species')  # all variables except Species standardized
#' 
#' # mtcars data, showing use with glm()
#' model1 <- glm(vs ~ wt + hp, data=mtcars, family='binomial')
#' beta(model1)  # wt and hp standardized, vs is not by default
#' 
#' # iris data, showing use with lme()
#' if (require(nlme, quietly=TRUE)) {
#'     model <- lme(Sepal.Width ~ Sepal.Length + Petal.Length, random=~1|Species, data=iris)
#'     beta(model)  # all three variables standardized
#' 
#'     beta(model, skip='Petal.Length')  # all variables except Petal.Length standardized
#' }
#' 
#' # iris data, showing use with lmer()
#' if (require(lme4, quietly=TRUE)) {
#'     model <- lmer(Sepal.Width ~ Sepal.Length + Petal.Length + (1|Species), data=iris)
#'     beta(model)  # all variables standardized
#' 
#'     beta(model, skip='Petal.Length')  # all variables except Petal.Length standardized
#' }
#' @export
beta <- function(model, ...) UseMethod('beta')


#' @describeIn beta Standardized coefficients for a linear model.
#' @export
beta.lm <- function(model, x=TRUE, y=TRUE, skip=NULL, ...) {
    call <- model$call
    vars <- names(model$model)
    formula <- format(formula(model))
    lhs <- attr(terms(model), 'response')  # index of criterion variable(s)
    if (x == FALSE) {
        vars <- vars[lhs]  # get only variables on left-hand side
    }
    if (y == FALSE) {
        vars <- vars[-lhs]  # get only variables on right-hand side
    }
    
    # cover special case, where all variables are skipped
    if (length(vars) == 0) {
        return(summary(model))
    }
    
    formula <- .create_formula(model, vars, skip)
    data <- formula[['data']]
    call[['formula']] <- formula[['formula']]
    call[['data']] <- quote(data)  # need this so data doesn't get output
                                   # directly in the summary
    return(summary(eval(call)))
}


#' @describeIn beta Standardized coefficients for ANOVA.
#' @export
beta.aov <- function(model, x=TRUE, y=TRUE, skip=NULL, ...) {
    model$call[[1]] <- quote(lm)  # need to change this so lm is returned
                                  # instead of aov
    beta.lm(model, x, y, skip)
}


#' @describeIn beta Standardized coefficients for a generalized linear model.
#' @export
beta.glm <- function(model, x=TRUE, y=FALSE, skip=NULL, ...) {
    beta.lm(model, x, y, skip)
}


#' @describeIn beta Standardized coefficients for a hierarchical linear model (nlme).
#' @export
beta.lme <- function(model, x=TRUE, y=TRUE, skip=NULL, ...) {
    call <- model$call
    vars <- names(model$data)
    formula <- format(formula(model))
    lhs <- attr(terms(model), 'response')  # index of criterion variable(s)
    if (x == FALSE) {
        vars <- vars[lhs]  # get only variables on left-hand side
    }
    if (y == FALSE) {
        vars <- vars[-lhs]  # get only variables on right-hand side
    }
    
    # cover special case, where all variables are skipped
    if (length(vars) == 0) {
        return(summary(model))
    }
    
    formula <- .create_formula(model, vars, skip)
    data <- formula[['data']]
    call[['fixed']] <- as.formula(formula[['formula']])
    call[['data']] <- quote(data)  # need this so data doesn't get output
                                   # directly in the summary
    return(summary(eval(call)))
}


#' @describeIn beta Standardized coefficients for a hierarchical linear model (lme4).
#' @export
beta.merMod <- function(model, x=TRUE, y=TRUE, skip=NULL, ...) {
    call <- model@call
    vars <- names(model@frame)
    formula <- format(formula(model))
    lhs <- attr(terms(model), 'response')  # index of criterion variable(s)
    
    # get all random variables, and unselect ones that are the DV
    randvars <- unlist(as.list(attr(terms(model@frame), 'predvars.random')[-1]))
    randvars <- randvars[-match(vars[lhs], randvars)]
    randvars <- vars[match(randvars, vars)]
        # turn variable symbols into character vector
    
    if (!is.null(skip)) {
        skip <- union(skip, randvars)
    } else {
        skip <- randvars
    }
    
    if (x == FALSE) {
        vars <- vars[lhs]  # get only variables on left-hand side
    }
    if (y == FALSE) {
        vars <- vars[-lhs]  # get only variables on right-hand side
    }
    
    # cover special case, where all variables are skipped
    if (length(vars) == 0) {
        return(summary(model))
    }
    
    formula <- .create_formula(model, vars, skip)
    data <- formula[['data']]
    call[['formula']] <- as.formula(formula[['formula']])
    call[['data']] <- quote(data)  # need this so data doesn't get output
                                   # directly in the summary
    return(summary(eval(call)))
}


#' Create formula for standardized estimates.
#' 
#' Helper function creates a new formula and data with scales variables.
#' 
#' @param model A fitted linear model.
#' @param vars Character vector with names of variables.
#' @param skip A string vector indicating any variables you do \emph{not} wish
#'   to be standarized.
#' @return Returns a list with new formula and new data.
#' @noRd
.create_formula <- function(model, vars, skip) {
    if(inherits(model, c('lm', 'aov', 'glm'))) {
        data <- model$model
    } else if (inherits(model, 'lme')) {
        data <- model$data
    } else if (inherits(model, 'lmerMod')) {
        data <- model@frame
    }
    
    formula <- format(formula(model))
    for (i in 1:length(vars)) {
        if (!(vars[i] %in% skip)) {
            
            # handle factor variables specially
            if (is.factor(data[, vars[i]])) {
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
                formula <- gsub(
                    paste0('(?<![[:alnum:]._])', vars[i], '(?![[:alnum:]._])'),
                    var_name, formula, perl=TRUE)
                    # includes lookbehind and lookahead to ensure that variables
                    # that are subsets of others (e.g., 'var' and 'thisisvar')
                    # won't get matched twice
                data[, var_name] <- scale(data[, vars[i]])
                    # add scaled variable to data frame
            }
        }
    }
    return(list(data=data, formula=formula))
}


