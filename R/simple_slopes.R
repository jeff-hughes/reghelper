#' Simple slopes of an interaction.
#' 
#' \code{simple_slopes} calculates all the simple effects of an interaction
#' in a fitted model (linear, generalized linear, hierarchical linear, or ANOVA).
#' 
#' If the model includes interactions at different levels (e.g., three two-way
#' interactions and one three-way interaction), the function will test the
#' simple effects of the highest-order interaction. If there are multiple
#' interactions in the highest order, it will test the first one in the model.
#' If you wish to test simple effects for a different interaction, simply switch
#' the order in the formula.
#' 
#' By default, this function will provide slopes at -1 SD, the mean, and +1 SD
#' for continuous variables, and at each level of categorical variables. This
#' can be overridden with the \code{levels} parameter.
#' 
#' If a categorical variable with more than two levels is being tested, you may
#' see multiple rows for that test. One row will be shown for each contrast for
#' that variable; the name of the contrast is identified in parentheses after 
#' the `sstest` label.
#' 
#' @param model A fitted linear model of type 'lm', 'glm', 'aov', 'lme' (nlme),
#'   or 'merMod' (lme4), with at least one interaction term.
#' @param levels A list with element names corresponding to some or all of the
#'   variables in the model. Each list element should be a vector with the names
#'   of factor levels (for categorical variables) or numeric values (for
#'   continuous variables) at which to test that variable. \strong{Note:} If you
#'   do not include 'sstest' as one of these levels, the function will not test
#'   the simple effects for that variable.
#' @param confint Whether or not to include confidence intervals for each estimate.
#' @param ci.width If `confint` is TRUE, this represents the width of the
#'   confidence intervals to calculate, as a proportion from 0 to 1.
#' @param confint.method For `merMod` models only, specifies what method to use
#'   for computing the confidence intervals.
#' @param ... Additional parameters to be passed on to the `confint` method, if
#'   `confint` is TRUE.
#' @return A data frame with a row for each simple effect. The first few columns
#'   identify the level at which each variable in your model was set for that
#'   test. A 'sstest' value in a particular column indicates that the simple
#'   slope for this variable was being tested. After columns for each variable,
#'   the data frame has columns for the slope of the test variable, the standard
#'   error, t-value, p-value, and degrees of freedom for the model. For `merMod`
#'   models, the degrees of freedom and p-values will not appear, as these are
#'   not calculated by the lme4 package.
#' @examples
#' # linear model
#' mtcars$am <- factor(mtcars$am)  # make 'am' categorical
#' model <- lm(mpg ~ wt * am, data=mtcars)
#' summary(model)  # significant interaction
#' simple_slopes(model)
#' simple_slopes(model,
#'     levels=list(wt=c(2, 3, 4, 'sstest'), am=c(0, 1, 'sstest')))  # test at specific levels
#' 
#' # generalized linear model
#' model <- glm(vs ~ gear * wt, data=mtcars, family='binomial')
#' summary(model)  # marginal interaction
#' simple_slopes(model)
#' simple_slopes(model,
#'     levels=list(gear=c(2, 3, 4, 'sstest'), wt=c(2, 3, 'sstest')))  # test at specific levels
#' 
#' # hierarchical linear model (nlme)
#' if (require(nlme, quietly=TRUE)) {
#'     model <- lme(Sepal.Width ~ Sepal.Length * Petal.Length, random=~1|Species, data=iris)
#'     summary(model)  # significant interaction
#'     simple_slopes(model)
#'     simple_slopes(model,
#'         levels=list(Sepal.Length=c(4, 5, 6, 'sstest'),
#'         Petal.Length=c(2, 3, 'sstest')))  # test at specific levels
#' }
#' 
#' # hierarchical linear model (lme4)
#' if (require(lme4, quietly=TRUE)) {
#'     model <- lmer(Sepal.Width ~ Sepal.Length * Petal.Length + (1|Species), data=iris)
#'     summary(model)
#'     simple_slopes(model)
#'     simple_slopes(model,
#'         levels=list(Sepal.Length=c(4, 5, 6, 'sstest'),
#'         Petal.Length=c(2, 3, 'sstest')))  # test at specific levels
#' }
#' @export
simple_slopes <- function(model, ...) UseMethod('simple_slopes')


#' @describeIn simple_slopes Simple slopes for linear models.
#' @import MASS
#' @export
simple_slopes.lm <- function(model, levels=NULL, confint=FALSE, ci.width=0.95, ...) {
    call <- model$call
    mdata <- model$model
    
    int_term <- which.max(attr(terms(model), 'order'))
        # get location of highest interaction term
    int_vars <- names(which(attr(terms(model), 'factors')[, int_term] == 1))
        # get location of variables in the interaction
    
    # figure out which variables are categorical, and pull their contrasts;
    # we also deal with character vectors here, which are normally silently
    # converted to factors when used in a model
    factor_vars <- c()
    original_contrasts <- list()
    for (v in 1:length(int_vars)) {
        var <- int_vars[v]
        if (is.character(mdata[, var])) {
            mdata[, var] <- factor(mdata[, var])
        }
        if (is.factor(mdata[, var])) {
            factor_vars <- c(factor_vars, var)
            original_contrasts[[var]] <- contrasts(mdata[, var])
        }
    }
    
    # get points at which to test each variable
    factors <- .set_factors(mdata, int_vars, levels)
    
    # create grid of all tests to be done
    grids <- .create_grids(mdata, factors)
    
    form <- format(formula(model))
    
    template <- grids[[1]]
    models <- grids[[2]]
    
    if(confint){
        lower.name <- paste(100*(1-ci.width)/2,"%",sep="")
        upper.name <- paste(100*(1+ci.width)/2,"%",sep="")
        models[, c('Test Estimate', 'Std. Error', 't value', 'Pr(>|t|)', 'df', lower.name, upper.name)] <- NA
    }
    else{
        models[, c('Test Estimate', 'Std. Error', 't value', 'Pr(>|t|)', 'df')] <- NA
    }

    est_count <- 1

    for (i in 1:nrow(template)) {
        new_form <- form
        test_var_name <- names(template)[which(startsWith(as.character(template[i, ]), 'sstest'))]
        test_var <- mdata[[test_var_name]]
        
        for (j in 1:ncol(template)) {
            vname <- colnames(template)[j]
            if (vname != test_var_name) {
                if (is.factor(mdata[[vname]])) {
                    # for factors, we set the contrast, with reference group as
                    # the one for that test
                    contrasts(mdata[[vname]]) <- contr.treatment(
                        levels(mdata[[vname]]),
                        base=which(levels(mdata[[vname]]) == template[i, j])
                    )
                } else {
                    # for continuous, we replace the name of the variable in the
                    # formula to shift the 0 point
                    new_var <- paste0('I(', vname, ' - ', template[i, j], ')')
                    new_form <- gsub(
                        paste0("((?<=[^a-zA-Z0-9._])|^)", vname, "(?=([^a-zA-Z0-9._]|$))"),
                        new_var, new_form, perl=TRUE)
                        # this regex uses lookarounds to ensure that it does not
                        # match a variable name that is a subset of another
                        # variable name (e.g. two variables named "var1" and "var11")
                }
            } else {
                # when testing a factor effect, revert to original contrasts
                # that the user had set
                if (is.factor(mdata[[vname]])) {
                    contrasts(mdata[[vname]]) <- original_contrasts[[vname]]
                }
            }
        }
        call[['formula']] <- new_form
        call[['data']] <- quote(mdata)
        if(!is.null(call[['weights']])) {
            call[['weights']] <- quote(`(weights)`)
        }
        new_model <- eval(call)
        
        if(confint){
            new_confint <- confint(new_model, level = ci.width, ...)
        }
        
        if (is.factor(test_var)) {
            contr <- original_contrasts[[test_var_name]]
            
            if (!is.null(colnames(contr))) {
                dummy_names <- paste0(test_var_name, colnames(contr))
            } else {
                dummy_names <- paste0(test_var_name, 1:ncol(contr))
            }
            
            estimates <- as.data.frame(
                summary(new_model)$coefficients[dummy_names, , drop=FALSE])
            estimates$df <- new_model$df.residual
            if (confint) {
                estimates <- cbind(estimates, new_confint[dummy_names, , drop=FALSE])
            }
            
            for (est in 1:nrow(estimates)) {
                models[est_count, (ncol(models)-ncol(estimates)+1):ncol(models)] <- estimates[est, ]
                est_count <- est_count + 1
            }
        } else {
            estimate <- c(summary(new_model)$coefficients[test_var_name, ], new_model$df.residual)
            if (confint) {
                estimate <- c(estimate, new_confint[test_var_name, ])
            }

            models[est_count, (ncol(models)-length(estimate)+1):ncol(models)] <- estimate
            est_count <- est_count + 1
        }
    }
    
    # adjust order of columns; should always be:
    # Test Estimate, Std. Error, [CI low, CI high,] t value, df, Pr(>|t|)
    if (confint) {
        col_order <- c('Test Estimate', 'Std. Error', lower.name, upper.name, 't value', 'df', 'Pr(>|t|)')
    } else {
        col_order <- c('Test Estimate', 'Std. Error', 't value', 'df', 'Pr(>|t|)')
    }
    var_names <- colnames(models)[1:(ncol(models)-length(col_order))]
    models <- models[, c(var_names, col_order)]
    
    class(models) <- c('simple_slopes', 'data.frame')
    return(models)
}


#' @describeIn simple_slopes Simple slopes for generalized linear models.
#' @export
simple_slopes.glm <- function(model, levels=NULL, confint=FALSE, ci.width=0.95, ...) {
    simple_slopes.lm(model, levels, confint, ci.width, ...)
}


#' @describeIn simple_slopes Simple slopes for hierarchical linear models (nlme).
#' @export
simple_slopes.lme <- function(model, levels=NULL, confint=FALSE, ci.width=0.95, ...) {
    call <- model$call
    mdata <- model$data
    
    int_term <- which.max(attr(terms(model), 'order'))
        # get location of highest interaction term
    int_vars <- names(which(attr(terms(model), 'factors')[, int_term] == 1))
        # get location of variables in the interaction
    
    # figure out which variables are categorical
    factor_vars_log <- vapply(int_vars, function(v) {
        is.factor(mdata[, v])
    }, logical(1))
    factor_vars <- names(factor_vars_log)[which(factor_vars_log == 1)]
    
    original_contrasts <- list()
    if (length(factor_vars) > 0) {
        for (i in 1:length(factor_vars)) {
            original_contrasts[[factor_vars[i]]] <- contrasts(mdata[, factor_vars[i]])
        }
    }
    
    # get points at which to test each variable
    factors <- .set_factors(mdata, int_vars, levels)
    
    # create grid of all tests to be done
    grids <- .create_grids(mdata, factors)
    
    form <- format(formula(model))
    form <- paste(trimws(form), collapse=" ")
    
    template <- grids[[1]]
    models <- grids[[2]]
    
    if(confint){
        lower.name <- paste(100*(1-ci.width)/2,"%",sep="")
        upper.name <- paste(100*(1+ci.width)/2,"%",sep="")
        models[, c('Test Estimate', 'Std. Error', 'df', 't value', 'Pr(>|t|)', lower.name, upper.name)] <- NA
    }
    else{
        models[, c('Test Estimate', 'Std. Error', 'df', 't value', 'Pr(>|t|)')] <- NA
    }
    est_count <- 1
    
    for (i in 1:nrow(template)) {
        new_form <- form
        test_var_name <- names(template)[which(startsWith(as.character(template[i, ]), 'sstest'))]
        test_var <- mdata[[test_var_name]]
        
        for (j in 1:ncol(template)) {
            vname <- colnames(template)[j]
            if (vname != test_var_name) {
                if (is.factor(mdata[[vname]])) {
                    # for factors, we set the contrast, with reference group as
                    # the one for that test
                    contrasts(mdata[[vname]]) <- contr.treatment(
                        levels(mdata[[vname]]),
                        base=which(levels(mdata[[vname]]) == template[i, j])
                    )
                } else {
                    # for continuous, we replace the name of the variable in the
                    # formula to shift the 0 point
                    new_var <- paste0('I(', vname, ' - ', template[i, j], ')')
                    new_form <- gsub(vname, new_var, new_form)
                }
            } else {
                # when testing a factor effect, revert to original contrasts
                # that the user had set
                if (is.factor(mdata[[vname]])) {
                    contrasts(mdata[[vname]]) <- original_contrasts[[vname]]
                }
            }
        }
        call[['fixed']] <- as.formula(new_form)
        call[['data']] <- quote(mdata)
        new_model <- eval(call)
        
        if(confint){
            #new_confint <- confint(new_model,level = ci.width, confint.method=confint.method, ...) #confint method not implemented
            #recommended alternative:
            new_confint <- nlme::intervals(new_model,level=ci.width,which = "fixed",...)$fixed[,-2]
        }
        
        if (is.factor(test_var)) {
            contr <- original_contrasts[[test_var_name]]
            dummy_names <- paste0(test_var_name, colnames(contr))
            
            estimates <- as.data.frame(
                summary(new_model)$tTable[dummy_names, ])
            
            # when only one contrast, the coefficients will be a vector, not a
            # matrix, so estimates ends up transposed
            if (ncol(contr) < 2) {
                estimates <- as.data.frame(t(estimates))
                rownames(estimates) <- 1
            }
            
            if (confint) {
                estimates <- cbind(estimates, new_confint[dummy_names, , drop=FALSE])
            }
            
            for (est in 1:nrow(estimates)) {
                models[est_count, (ncol(models)-ncol(estimates)+1):ncol(models)] <- estimates[est, ]
                est_count <- est_count + 1
            }
        } else {
            estimate <- summary(new_model)$tTable[test_var_name, ]
            if (confint) {
                estimate <- c(estimate, new_confint[test_var_name, ])
            }
            
            models[est_count, (ncol(models)-length(estimate)+1):ncol(models)] <- estimate
            est_count <- est_count + 1
        }
    }
    
    # adjust order of columns; should always be:
    # Test Estimate, Std. Error, [CI low, CI high,] t value, df, Pr(>|t|)
    if (confint) {
        col_order <- c('Test Estimate', 'Std. Error', lower.name, upper.name, 't value', 'df', 'Pr(>|t|)')
    } else {
        col_order <- c('Test Estimate', 'Std. Error', 't value', 'df', 'Pr(>|t|)')
    }
    var_names <- colnames(models)[1:(ncol(models)-length(col_order))]
    models <- models[, c(var_names, col_order)]
    
    class(models) <- c('simple_slopes', 'data.frame')
    return(models)
}


#' @describeIn simple_slopes Simple slopes for hierarchical linear models (lme4).
#' @export
simple_slopes.merMod <- function(
    model,
    levels=NULL,
    confint=FALSE,
    ci.width=0.95,
    confint.method=c("Wald", "profile", "boot"),
    ...) {
    
    call <- model@call
    mdata <- model@frame
    confint.method <- match.arg(confint.method)

    int_term <- which.max(attr(terms(model), 'order'))
        # get location of highest interaction term
    int_vars <- names(which(attr(terms(model), 'factors')[, int_term] == 1))
        # get location of variables in the interaction
    
    # figure out which variables are categorical
    factor_vars_log <- vapply(int_vars, function(v) {
        is.factor(mdata[, v])
    }, logical(1))
    factor_vars <- names(factor_vars_log)[which(factor_vars_log == 1)]
    
    original_contrasts <- list()
    if (length(factor_vars) > 0) {
        for (i in 1:length(factor_vars)) {
            original_contrasts[[factor_vars[i]]] <- contrasts(mdata[, factor_vars[i]])
        }
    }

    # get points at which to test each variable
    factors <- .set_factors(mdata, int_vars, levels)
    
    # create grid of all tests to be done
    grids <- .create_grids(mdata, factors)
    
    form <- format(formula(model))
    
    template <- grids[[1]]
    models <- grids[[2]]
    
    # distinguish between lmer and lmerTest models
    if ('lmerModLmerTest' %in% class(model)) {
        column_names <- c('Test Estimate', 'Std. Error', 'df', 't value', 'Pr(>|t|)')
    } else {
        column_names <- c('Test Estimate', 'Std. Error', 't value')
    }

    if(confint){
        lower.name <- paste(100*(1-ci.width)/2,"%",sep="")
        upper.name <- paste(100*(1+ci.width)/2,"%",sep="")
        column_names <- c(column_names, lower.name, upper.name)
    }
    models[, column_names] <- NA
    est_count <- 1
    
    for (i in 1:nrow(template)) {
        new_form <- form
        test_var_name <- names(template)[which(startsWith(as.character(template[i, ]), 'sstest'))]
        test_var <- mdata[[test_var_name]]
        
        for (j in 1:ncol(template)) {
            vname <- colnames(template)[j]
            if (vname != test_var_name) {
                if (is.factor(mdata[[vname]])) {
                    # for factors, we set the contrast, with reference group as
                    # the one for that test
                    contrasts(mdata[[vname]]) <- contr.treatment(
                        levels(mdata[[vname]]),
                        base=which(levels(mdata[[vname]]) == template[i, j])
                    )
                } else {
                    # for continuous, we replace the name of the variable in the
                    # formula to shift the 0 point
                    new_var <- paste0('I(', vname, ' - ', template[i, j], ')')
                    new_form <- gsub(vname, new_var, new_form)
                }
            } else {
                # when testing a factor effect, revert to original contrasts
                # that the user had set
                if (is.factor(mdata[[vname]])) {
                    contrasts(mdata[[vname]]) <- original_contrasts[[vname]]
                }
            }
        }
        
        call[['formula']] <- as.formula(new_form)
        call[['data']] <- quote(mdata)
        if(!is.null(call[['weights']])) {
            call[['weights']] <- quote(`(weights)`)
        }
        new_model <- eval(call)
        
        if(confint){
            new_confint <- confint(new_model,method=confint.method,level = ci.width, ...)
        }
        
        if (is.factor(test_var)) {
            contr <- original_contrasts[[test_var_name]]
            dummy_names <- paste0(test_var_name, colnames(contr))
            
            estimates <- as.data.frame(
                coef(summary(new_model))[dummy_names, ])
            
            # when only one contrast, the coefficients will be a vector, not a
            # matrix, so estimates ends up transposed
            if (ncol(contr) < 2) {
                estimates <- as.data.frame(t(estimates))
                rownames(estimates) <- 1
            }
            
            if (confint) {
                estimates <- cbind(estimates, new_confint[dummy_names, , drop=FALSE])
            }
            
            for (est in 1:nrow(estimates)) {
                models[est_count, (ncol(models)-ncol(estimates)+1):ncol(models)] <- estimates[est, ]
                est_count <- est_count + 1
            }
        } 
        else {
            estimate <- coef(summary(new_model))[test_var_name, ]
            if (confint) {
                estimate <- c(estimate, new_confint[test_var_name, ])
            }
            models[est_count, (ncol(models)-length(estimate)+1):ncol(models)] <- estimate
            est_count <- est_count + 1
        }
    }
    
    # adjust order of columns; should always be:
    # Test Estimate, Std. Error, [CI low, CI high,] t value, df, Pr(>|t|)
    if ('lmerModLmerTest' %in% class(model)) {
        col_order <- c('Test Estimate', 'Std. Error', 'df', 't value', 'Pr(>|t|)')
    } else {
        col_order <- c('Test Estimate', 'Std. Error', 't value')
    }
    if (confint) {
        col_order <- c(col_order[1:2], lower.name, upper.name, col_order[3:length(col_order)])
    }
    var_names <- colnames(models)[1:(ncol(models)-length(col_order))]
    models <- models[, c(var_names, col_order)]

    class(models) <- c('simple_slopes', 'data.frame')
    return(models)
}


#' Print simple slopes.
#' 
#' \code{print} method for class "\code{simple_slopes}".
#' 
#' @param x An object of class "\code{simple_slopes}", usually, a result
#'   of a call to \code{\link{simple_slopes}}.
#' @param digits The number of significant digits to use when printing.
#' @param signif.stars Logical. If \code{TRUE}, 'significance stars' are printed
#'   for each coefficient.
#' @param ... Further arguments passed to or from other methods.
#' @seealso \code{\link{simple_slopes}}
#' @export
print.simple_slopes <- function(
    x, 
    digits=max(3L, getOption('digits') - 3L), 
    signif.stars=getOption('show.signif.stars'), 
    ...){
    
    model <- x
    
    if (!is.logical(signif.stars) || is.na(signif.stars)) {
        warning("option \"show.signif.stars\" is invalid: assuming TRUE")
        signif.stars <- TRUE
    }
    
    index <- c('Test Estimate', 'Std. Error',colnames(model)[endsWith(colnames(model),"%")], 't value', 'df')
    for (i in index) {
        if (i %in% colnames(model)) {
            model[, i] <- round(model[, i], digits=digits)
        }
    }
    
    if ('Pr(>|t|)' %in% colnames(model)) {
        if (signif.stars) {
            stars <- symnum(as.numeric(model[, 'Pr(>|t|)']), corr=FALSE, na=FALSE,
                numeric.x=TRUE, cutpoints=c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                symbols=c("***", "**", "*", ".", " "))
        }
        
        model[, 'Pr(>|t|)'] <- format.pval(as.numeric(model[, 'Pr(>|t|)']), digits=digits)
        
        if (signif.stars) {
            model[, 'Sig.'] <- as.character(stars)
        }
    }
    
    print.data.frame(model, quote=FALSE, right=TRUE, na.print='NA')
    invisible(model)
}


#' Create grid of simple slope tests.
#' 
#' Helper function creates a data frame with a list of all simple slope tests to
#' be done.
#' 
#' @param data Data from the linear model being tested.
#' @param factors List of all points to test for each variable in the
#'   interaction.
#' @return A data frame with one line for each simple slope test, indicating
#'   what point each variable is set to in the test.
#' @noRd
.create_grids <- function(data, factors) {
    grid <- with(data, expand.grid(factors))
    
    # we only want to use the models that are testing a single variable
    find_tests <- apply(grid, 1, function(x) {
        length(which(suppressWarnings(x == 'sstest'))) == 1
    })
    grid <- grid[find_tests, ]
    
    rownames(grid) <- seq(to=nrow(grid))
    
    # remove factor levels from factor variables
    grid <- as.data.frame(lapply(grid, function(x) {
        as.character(x)
    }), stringsAsFactors=FALSE)
    
    new_grid <- grid
    
    # look for factor variables with more than 2 levels -- they need extra rows
    for (var in names(factors)) {
        variable <- data[[var]]
        if (is.factor(variable) && length(levels(variable)) > 2) {
            find_rows <- which(new_grid[, var] == 'sstest')
            contr <- contrasts(variable)
            
            # count up number of times we should be repeating each row
            num_rep <- ifelse(1:nrow(new_grid) %in% find_rows, ncol(contr), 1)
            rep_index <- rep(row.names(new_grid), num_rep)
            
            new_grid <- new_grid[rep_index, ]
            
            # change rownames to letter subscripts, e.g., '4a', '4b'
            dupe_values <- data.frame(table(rep_index))
            dupe_values <- dupe_values[dupe_values$Freq > 1, ]
            for (i in dupe_values$rep_index) {
                indices <- which(rep_index == as.character(i))
                new_grid[indices, var] <- paste0('sstest (', colnames(contr), ')')
            }
            rownames(new_grid) <- 1:nrow(new_grid)
        }
    }
    
    return(list(grid, new_grid))
}



