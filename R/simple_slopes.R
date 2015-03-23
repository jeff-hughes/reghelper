#' Simple slopes of interaction.
#' 
#' \code{simple_slopes} is a generic function for calculating the simple effects
#' of an interaction in a regression model.
#' 
#' @param model A fitted linear model of type 'lm', 'glm', 'aov', 'lme' (nlme),
#'   or 'merMod' (lme4).
#' @param ... Additional arguments to be passed to the particular method for the
#'   given model.
#' @return The form of the value returned by \code{simple_slopes} depends on the
#'   class of its argument. See the documentation of the particular methods for
#'   details of what is produced by that method.
#' @seealso \code{\link{simple_slopes.lm}}, \code{\link{simple_slopes.glm}},
#'   \code{\link{simple_slopes.aov}}, \code{\link{simple_slopes.lme}},
#'   \code{\link{simple_slopes.merMod}}
#' @examples
#' # mtcars data
#' mtcars$am <- factor(mtcars$am)  # make 'am' categorical
#' model <- lm(mpg ~ wt * am, data=mtcars)
#' summary(model)  # significant interaction
#' simple_slopes(model)
#' simple_slopes(model,
#'     levels=list(wt=c(2, 3, 4, 'sstest'), am=c(0, 1, 'sstest')))  # test at specific levels
#' @export
simple_slopes <- function(model, ...) UseMethod('simple_slopes')


#' Simple slopes of interaction.
#' 
#' \code{simple_slopes.lm} calculates all the simple effects of an interaction
#' in a regression model.
#' 
#' If the model includes interactions at different levels (e.g., three two-way
#' interactions and one three-way interaction), the function will test the
#' simple effects of the highest-order interaction. If there are multiple
#' interactions in the highest order, it will test the first one in the model.
#' If you wish to test simple effects for a different interaction, simply switch
#' the order in the formula.
#' 
#' By default, this function will provide slopes at -1SD, the mean, and +1SD for
#' continuous variables, and at each level of categorical variables. This can be
#' overridden with the \code{levels} parameter.
#' 
#' If a categorical variable with more than two levels is being tested, you may
#' see multiple row for that test. One row will be shown for each contrast for
#' that variable; the order is in the same order shown in \code{contrasts()}.
#' 
#' @param model A fitted linear model of type 'lm' with at least one interaction
#'   term.
#' @param levels A list with element names corresponding to some or all of the
#'   variables in the model. Each list element should be a vector with the names
#'   of factor levels (for categorical variables) or numeric points (for
#'   continuous variables) at which to test that variable. \strong{Note:} If you
#'   do not include 'sstest' as one of these levels, the function will not test
#'   the simple effects for that variable.
#' @return A data frame with a row for each simple effect. The first few columns
#'   identify the level at which each variable in your model was set for that
#'   test. A 'sstest' value in a particular column indicates that this was the
#'   variable being tested. After columns for each variable, the data frame has
#'   columns for the slope of the test variable, the standard error, t-value,
#'   p-value, and degrees of freedom for the model.
#' @seealso \code{\link{simple_slopes.glm}},\code{\link{simple_slopes.lme}},
#'   \code{\link{simple_slopes.merMod}}
#' @examples
#' # mtcars data
#' mtcars$am <- factor(mtcars$am)  # make 'am' categorical
#' model <- lm(mpg ~ wt * am, data=mtcars)
#' summary(model)  # significant interaction
#' simple_slopes(model)
#' simple_slopes(model,
#'     levels=list(wt=c(2, 3, 4, 'sstest'), am=c(0, 1, 'sstest')))  # test at specific levels
#' @export
simple_slopes.lm <- function(model, levels=NULL) {
    call <- model$call
    mdata <- model$model
    
    int_term <- which.max(attr(terms(model), 'order'))
        # get location of highest interaction term
    int_vars <- names(which(attr(terms(model), 'factors')[, int_term] == 1))
        # get location of variables in the interaction
    
    # get points at which to test each variable
    factors <- .set_factors(mdata, int_vars, levels)
    
    # create grid of all tests to be done
    grids <- .create_grids(mdata, factors)
    
    form <- format(formula(model))
    
    template <- grids[[1]]
    models <- grids[[2]]
    models[, c('Test Estimate', 'Std. Error',
               't value', 'Pr(>|t|)', 'df')] <- NA
    est_count <- 1
    
    for (i in 1:nrow(template)) {
        new_form <- form
        test_var_name <- names(template)[which(template[i, ] == 'sstest')]
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
            }
        }
        call[['formula']] <- new_form
        call[['data']] <- quote(mdata)
        new_model <- eval(call)
        
        if (is.factor(test_var)) {
            contr <- contrasts(test_var)
            dummy_names <- paste0(test_var_name, colnames(contr))
            
            estimates <- as.data.frame(
                summary(new_model)$coefficients[dummy_names, ])
            
            # when only one contrast, the coefficients will be a vector, not a
            # matrix, so estimates ends up transposed
            if (ncol(contr) < 2) {
                estimates <- as.data.frame(t(estimates))
                rownames(estimates) <- 1
            }
            
            estimates$df <- new_model$df.residual
            
            for (est in 1:nrow(estimates)) {
                models[est_count, (ncol(models)-4):ncol(models)] <- estimates[est, ]
                est_count <- est_count + 1
            }
        } else {
            models[est_count, (ncol(models)-4):ncol(models)] <- c(
                summary(new_model)$coefficients[test_var_name, ],
                new_model$df.residual
            )
            est_count <- est_count + 1
        }
    }
    
    # flip order so p-values come last
    columns <- ncol(models)
    models <- models[, c(1:(columns-2), columns, columns-1)]
    
    class(models) <- c('simple_slopes', 'data.frame')
    return(models)
}


#' Simple slopes of interaction.
#' 
#' \code{simple_slopes.aov} is an alias of simple_slopes.lm.
#' 
#' @seealso \code{\link{simple_slopes.lm}}
#' @export
simple_slopes.aov <- function(model, levels=NULL) {
    simple_slopes.lm(model, levels)
}


#' Simple slopes of interaction.
#' 
#' \code{simple_slopes.glm} calculates all the simple effects of an interaction
#' in a regression model.
#' 
#' If the model includes interactions at different levels (e.g., three two-way
#' interactions and one three-way interaction), the function will test the
#' simple effects of the highest-order interaction. If there are multiple
#' interactions in the highest order, it will test the first one in the model.
#' If you wish to test simple effects for a different interaction, simply switch
#' the order in the formula.
#' 
#' By default, this function will provide slopes at -1SD, the mean, and +1SD for
#' continuous variables, and at each level of categorical variables. This can be
#' overridden with the \code{levels} parameter.
#' 
#' If a categorical variable with more than two levels is being tested, you may
#' see multiple row for that test. One row will be shown for each contrast for
#' that variable; the order is in the same order shown in \code{contrasts()}.
#' 
#' @param model A fitted linear model of type 'glm' with at least one
#'   interaction term.
#' @param levels A list with element names corresponding to some or all of the
#'   variables in the model. Each list element should be a vector with the names
#'   of factor levels (for categorical variables) or numeric points (for
#'   continuous variables) at which to test that variable. \strong{Note:} If you
#'   do not include 'sstest' as one of these levels, the function will not test
#'   the simple effects for that variable.
#' @return A data frame with a row for each simple effect. The first few columns
#'   identify the level at which each variable in your model was set for that
#'   test. A 'sstest' value in a particular column indicates that this was the
#'   variable being tested. After columns for each variable, the data frame has
#'   columns for the slope of the test variable, the standard error, t-value,
#'   p-value, and degrees of freedom for the model.
#' @seealso \code{\link{simple_slopes.lm}}, \code{\link{simple_slopes.lme}},
#'   \code{\link{simple_slopes.merMod}}
#' @examples
#' # mtcars data
#' model <- glm(vs ~ gear * wt, data=mtcars, family='binomial')
#' summary(model)  # marginal interaction
#' simple_slopes(model)
#' simple_slopes(model,
#'     levels=list(gear=c(2, 3, 4, 'sstest'), wt=c(2, 3, 'sstest')))  # test at specific levels
#' @export
simple_slopes.glm <- function(model, levels=NULL) {
    simple_slopes.lm(model, levels)
}


#' Simple slopes of interaction.
#' 
#' \code{simple_slopes.lme} calculates all the simple effects of an interaction
#' in a regression model.
#' 
#' If the model includes interactions at different levels (e.g., three two-way
#' interactions and one three-way interaction), the function will test the
#' simple effects of the highest-order interaction. If there are multiple
#' interactions in the highest order, it will test the first one in the model.
#' If you wish to test simple effects for a different interaction, simply switch
#' the order in the formula.
#' 
#' By default, this function will provide slopes at -1SD, the mean, and +1SD for
#' continuous variables, and at each level of categorical variables. This can be
#' overridden with the \code{levels} parameter.
#' 
#' If a categorical variable with more than two levels is being tested, you may
#' see multiple row for that test. One row will be shown for each contrast for
#' that variable; the order is in the same order shown in \code{contrasts()}.
#' 
#' @param model A fitted linear model of type 'lme' with at least one
#'   interaction term.
#' @param levels A list with element names corresponding to some or all of the
#'   variables in the model. Each list element should be a vector with the names
#'   of factor levels (for categorical variables) or numeric points (for
#'   continuous variables) at which to test that variable. \strong{Note:} If you
#'   do not include 'sstest' as one of these levels, the function will not test
#'   the simple effects for that variable.
#' @return A data frame with a row for each simple effect. The first few columns
#'   identify the level at which each variable in your model was set for that
#'   test. A 'sstest' value in a particular column indicates that this was the
#'   variable being tested. After columns for each variable, the data frame has
#'   columns for the slope of the test variable, the standard error, t-value,
#'   p-value, and degrees of freedom for the model.
#' @seealso \code{\link{simple_slopes.lm}}, \code{\link{simple_slopes.glm}},
#'   \code{\link{simple_slopes.merMod}}
#' @examples
#' # iris data
#' model <- lme(Sepal.Width ~ Sepal.Length * Petal.Length, random=~1|Species, data=iris)
#' summary(model)  # significant interaction
#' simple_slopes(model)
#' simple_slopes(model,
#'     levels=list(Sepal.Length=c(4, 5, 6, 'sstest'), Petal.Length=c(2, 3, 'sstest')))  # test at specific levels
#' @export
simple_slopes.lme <- function(model, levels=NULL) {
    call <- model$call
    mdata <- model$data
    
    int_term <- which.max(attr(terms(model), 'order'))
        # get location of highest interaction term
    int_vars <- names(which(attr(terms(model), 'factors')[, int_term] == 1))
        # get location of variables in the interaction
    
    # get points at which to test each variable
    factors <- .set_factors(mdata, int_vars, levels)
    
    # create grid of all tests to be done
    grids <- .create_grids(mdata, factors)
    
    form <- format(formula(model))
    
    template <- grids[[1]]
    models <- grids[[2]]
    models[, c('Test Estimate', 'Std. Error',
        'df', 't value', 'Pr(>|t|)')] <- NA
    est_count <- 1
    
    for (i in 1:nrow(template)) {
        new_form <- form
        test_var_name <- names(template)[which(template[i, ] == 'sstest')]
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
            }
        }
        call[['fixed']] <- as.formula(new_form)
        call[['data']] <- quote(mdata)
        new_model <- eval(call)
        
        if (is.factor(test_var)) {
            contr <- contrasts(test_var)
            dummy_names <- paste0(test_var_name, colnames(contr))
            
            estimates <- as.data.frame(
                summary(new_model)$tTable[dummy_names, ])
            
            # when only one contrast, the coefficients will be a vector, not a
            # matrix, so estimates ends up transposed
            if (ncol(contr) < 2) {
                estimates <- as.data.frame(t(estimates))
                rownames(estimates) <- 1
            }
            
            for (est in 1:nrow(estimates)) {
                models[est_count,
                    (ncol(models)-4):ncol(models)] <- estimates[est, ]
                est_count <- est_count + 1
            }
        } else {
            models[est_count,
                (ncol(models)-4):ncol(models)
            ] <- summary(new_model)$tTable[test_var_name, ]
            est_count <- est_count + 1
        }
    }
    
    # flip order to keep consistent with simple_slopes.lm
    columns <- ncol(models)
    models <- models[, c(1:(columns-3), columns-1, columns-2, columns)]
    
    class(models) <- c('simple_slopes', 'data.frame')
    return(models)
}


#' Simple slopes of interaction.
#' 
#' \code{simple_slopes.merMod} calculates all the simple effects of an
#' interaction in a regression model.
#' 
#' If the model includes interactions at different levels (e.g., three two-way
#' interactions and one three-way interaction), the function will test the
#' simple effects of the highest-order interaction. If there are multiple
#' interactions in the highest order, it will test the first one in the model.
#' If you wish to test simple effects for a different interaction, simply switch
#' the order in the formula.
#' 
#' By default, this function will provide slopes at -1SD, the mean, and +1SD for
#' continuous variables, and at each level of categorical variables. This can be
#' overridden with the \code{levels} parameter.
#' 
#' If a categorical variable with more than two levels is being tested, you may
#' see multiple row for that test. One row will be shown for each contrast for
#' that variable; the order is in the same order shown in \code{contrasts()}.
#' 
#' @param model A fitted linear model of type 'merMod' with at least one
#'   interaction term.
#' @param levels A list with element names corresponding to some or all of the
#'   variables in the model. Each list element should be a vector with the names
#'   of factor levels (for categorical variables) or numeric points (for
#'   continuous variables) at which to test that variable. \strong{Note:} If you
#'   do not include 'sstest' as one of these levels, the function will not test
#'   the simple effects for that variable.
#' @return A data frame with a row for each simple effect. The first few columns
#'   identify the level at which each variable in your model was set for that
#'   test. A 'sstest' value in a particular column indicates that this was the
#'   variable being tested. After columns for each variable, the data frame has
#'   columns for the slope of the test variable, the standard error, and t-value
#'   for the model.
#' @seealso \code{\link{simple_slopes.lm}}, \code{\link{simple_slopes.glm}},
#'   \code{\link{simple_slopes.lme}}
#' @examples
#' # iris data
#' model <- lmer(Sepal.Width ~ Sepal.Length * Petal.Length + (1|Species), data=iris)
#' summary(model)
#' simple_slopes(model)
#' simple_slopes(model,
#'     levels=list(Sepal.Length=c(4, 5, 6, 'sstest'), Petal.Length=c(2, 3, 'sstest')))  # test at specific levels
#' @export
simple_slopes.merMod <- function(model, levels=NULL) {
    call <- model@call
    mdata <- model@frame
    
    int_term <- which.max(attr(terms(model), 'order'))
    # get location of highest interaction term
    int_vars <- names(which(attr(terms(model), 'factors')[, int_term] == 1))
    # get location of variables in the interaction
    
    # get points at which to test each variable
    factors <- .set_factors(mdata, int_vars, levels)
    
    # create grid of all tests to be done
    grids <- .create_grids(mdata, factors)
    
    form <- format(formula(model))
    
    template <- grids[[1]]
    models <- grids[[2]]
    models[, c('Test Estimate', 'Std. Error', 't value')] <- NA
    est_count <- 1
    
    for (i in 1:nrow(template)) {
        new_form <- form
        test_var_name <- names(template)[which(template[i, ] == 'sstest')]
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
            }
        }
        call[['formula']] <- as.formula(new_form)
        call[['data']] <- quote(mdata)
        new_model <- eval(call)
        
        if (is.factor(test_var)) {
            contr <- contrasts(test_var)
            dummy_names <- paste0(test_var_name, colnames(contr))
            
            estimates <- as.data.frame(
                coef(summary(new_model))[dummy_names, ])
            
            # when only one contrast, the coefficients will be a vector, not a
            # matrix, so estimates ends up transposed
            if (ncol(contr) < 2) {
                estimates <- as.data.frame(t(estimates))
                rownames(estimates) <- 1
            }
            
            for (est in 1:nrow(estimates)) {
                models[est_count,
                    (ncol(models)-2):ncol(models)] <- estimates[est, ]
                est_count <- est_count + 1
            }
        } else {
            models[est_count,
                (ncol(models)-2):ncol(models)
                ] <- coef(summary(new_model))[test_var_name, ]
            est_count <- est_count + 1
        }
    }
    class(models) <- c('simple_slopes', 'data.frame')
    return(models)
}


#' Print simple slopes.
#' 
#' \code{print} method for class "\code{simple_slopes}".
#' 
#' @param model An object of class "\code{simple_slopes}", usually, a result
#'   of a call to \code{\link{simple_slopes}}.
#' @param digits The number of significant digits to use when printing.
#' @param signif.stars Logical. If \code{TRUE}, 'significance stars' are printed
#'   for each coefficient.
#' @param ... Further arguments passed to or from other methods.
#' @seealso \code{\link{simple_slopes}}
#' @export
print.simple_slopes <- function(
    model,
    digits=max(3L, getOption('digits') - 3L),
    signif.stars=getOption('show.signif.stars'),
    ...) {

    suppressWarnings(printCoefmat(
        model,
        digits=digits,
        signif.stars=signif.stars,
        na.print='sstest'))
    invisible(model)
}


#' Find points to test variables.
#' 
#' Helper function calculates points at which to test slopes of a variable. For
#' categorical variables, this includes all its levels. For continuous
#' variables, this is -1 SD, the mean, and +1 SD.
#' 
#' @param model_data The data associated with the linear model being tested.
#' @param int_vars The variables involved in the interaction being tested.
#' @param user_levels Any user-specified levels to be used instead of the
#'   defaults.
#' @param sstest Logical. Whether or not to insert 'sstest' as a factor level.
#' @return A list with all the factor points for each variable in the
#'   interaction.
.set_factors <- function(model_data, int_vars, user_levels=NULL, sstest=TRUE) {
    factors <- list()
    for (term in int_vars) {
        term_data <- model_data[[term]]
        if (!is.null(user_levels) && term %in% names(user_levels)) {
            # if user specified levels, use those
            factors[[term]] <- user_levels[[term]]
        } else {
            if (is.factor(term_data)) {
                # factors are plotted at all levels
                if (sstest == TRUE) {
                    factors[[term]] <- c('sstest', levels(term_data))
                } else {
                    factors[[term]] <- levels(term_data)
                }
            } else {
                # continuous vars are plotted at -1SD, mean, and +1 SD
                if (sstest == TRUE) {
                    factors[[term]] <- c(
                        'sstest',
                        .offset_point(term_data, -1),
                        .offset_point(term_data, 0),
                        .offset_point(term_data, 1)
                    )
                } else {
                    factors[[term]] <- c(
                        .offset_point(term_data, -1),
                        .offset_point(term_data, 0),
                        .offset_point(term_data, 1)
                    )
                }
            }
        }
    }
    return(factors)
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
            find_rows <- which(grid[, var] == 'sstest')
            contr <- contrasts(variable)
            
            adj_find_rows <- find_rows +
                (ncol(contr)-1) * 0:(length(find_rows)-1)
                # adjust find_rows, since we are going to be adding rows in
                # between as we loop through
            
            for (row in 1:length(find_rows)) {
                old_row <- grid[find_rows[row], ]
                new_rows <- as.data.frame(t(sapply(1:ncol(contr), function(x) {
                    old_row
                })))  # kind of hacky way to copy rows
                
                # change rownames to letter subscripts, e.g., '4a', '4b'
                rownames(new_rows) <- paste0(
                    rownames(old_row),
                    letters[1:nrow(new_rows)])
                
                new_grid <- .df_row_splice(
                    new_grid,
                    adj_find_rows[row],
                    num_remove=1,
                    new_rows=new_rows)
            }
        }
    }
    
    return(list(grid, new_grid))
}




