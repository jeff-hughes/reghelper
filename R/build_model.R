#' Incremental block modelling.
#' 
#' \code{build_model} allows you to incrementally add terms to a linear regression
#' model.
#' 
#' Given a list of names of variables at each step, this function will run a
#' series of models, adding the terms for each block incrementally.
#' 
#' @param dv The variable name to be used as the dependent variable.
#' @param ... Pass through variable names (or interaction terms) to add for each
#'   block. To add one term to a block, just pass it through directly; to
#'   add multiple terms, pass it through in a vector or list. Blocks will be
#'   added in the order they are passed to the function, and variables from
#'   previous blocks will be added to each subsequent block.
#' @param data An optional data frame containing the variables in the model. If
#'   not found in \code{data}, the variables are taken from the environment from
#'   which the function is called.
#' @param opts List of arguments to be passed to the model function.
#' @param model The type of model to use; only supports 'lm' at this time.
#' @return A named list with the following elements:
#' \tabular{ll}{
#'   \code{formulas} \tab A list of the regression formulas used for each block.
#'   \cr
#'   \code{models} \tab A list of all regression models.\cr
#' }
#' @examples
#' # 2 blocks: Petal.Length; Petal.Length + Petal.Width
#' model1 <- build_model(Sepal.Length, Petal.Length, Petal.Width, data=iris, model='lm')
#' summary(model1)
#' coef(model1)
#' 
#' # 2 blocks: Species; Species + Petal.Length + Petal.Width + Petal.Length:Petal.Width
#' model2 <- build_model(Sepal.Length, Species, c(Petal.Length * Petal.Width), data=iris, model='lm')
#' summary(model2)
#' coef(model2)
#' @export
build_model <- function(dv, ..., data=NULL, opts=NULL, model='lm') {
    # grab blocks of variable names
    call_list <- as.list(match.call())[-1]
    call_list[which(names(call_list) %in% c('dv', 'data', 'opts', 'model'))] <- NULL
    
    if (length(call_list) > 0) {
        # turn variable names into strings
        blocks <- sapply(call_list, deparse)
        
        # because we can have vectors or lists of variables designating a block,
        # we need to pull out the individual variable names and store in a list
        match1 <- regmatches(blocks, regexec('^(?:c|list)\\((.*)\\)$', blocks))
            # matches a vector or list, pulls out contents inside
        vars <- list()
        for (i in 1:length(match1)) {
            if (length(match1[[i]]) > 0) {
                match2 <- regmatches(
                    match1[[i]][-1],
                    gregexpr('([^ ,](?:[^,])*[^ ,])', match1[[i]][-1]))
                    # matches comma-separated values, removes whitespace
                vars[[i]] <- match2[[1]]
            } else {
                vars[[i]] <- blocks[[i]]
            }
        }
    } else {
        vars <- 1  # just the intercept
    }
    block_q <- list()
    for (i in 1:length(vars)) {
        predictors <- ''
        for (j in 1:i) {  # include all previous blocks
            if (j == 1) {
                predictors <- paste(vars[[j]], collapse=' + ')
            } else {
                predictors <- paste0(
                    predictors,
                    ' + ',
                    paste(vars[[j]], collapse=' + ')
                )
            }
        }
        block_q[[i]] <- predictors
    }
    models <- build_model_q(deparse(substitute(dv)), block_q, data, opts=opts, model=model)
    return(models)
}


#' Incremental block modelling.
#' 
#' \code{build_model_q} allows you to incrementally add terms to a linear
#' regression model.
#' 
#' Given a list of names of variables at each step, this function will run a
#' series of models, adding the terms for each block incrementally. Note that in
#' most cases it is easier to use \code{\link{build_model}} and pass variable names
#' in directly instead of strings of variable names. \code{build_model_q} uses
#' standard evaluation in cases where such evaluation is easier.
#' 
#' @param dv String of the variable name to be used as the dependent variable.
#' @param blocks List of variable names (or interaction terms) to add for each
#'   block. Each list element should be a vector or list of strings with terms
#'   for that block. Variables from previous blocks will be added to each
#'   subsequent block.
#' @param data An optional data frame containing the variables in the model. If
#'   not found in \code{data}, the variables are taken from the environment from
#'   which the function is called.
#' @param opts List of arguments to be passed to the model function.
#' @param model The type of model to use; only supports 'lm' at this time.
#' @return A named list with the following elements:
#' \tabular{ll}{
#'   \code{formulas} \tab A list of the regression formulas used for each block.
#'   \cr
#'   \code{models} \tab A list of all regression models.\cr
#' }
#' @seealso \code{\link{build_model}}
#' @examples
#' # 2 blocks: Petal.Length; Petal.Length + Petal.Width
#' model1 <- build_model_q('Sepal.Length', list('Petal.Length', 'Petal.Width'), data=iris, model='lm')
#' summary(model1)
#' coef(model1)
#' 
#' # 2 blocks: Species; Species + Petal.Length + Petal.Width + Petal.Length:Petal.Width
#' model2 <- build_model_q('Sepal.Length', list('Species', 'Petal.Length * Petal.Width')), data=iris, model='lm')
#' summary(model2)
#' coef(model2)
#' @export
build_model_q <- function(dv, blocks=NULL, data=NULL, opts=NULL, model='lm') {
    formulas <- list()
    models <- list()
    
    for (i in 1:length(blocks)) {
        formulas[[i]] <- as.formula(paste(dv, '~', blocks[[i]]))
        
        if (is.null(opts)) {
            args <- list(formula=formulas[[i]], data=data)
        } else {
            args <- c(list(formula=formulas[[i]], data=data), opts)
        }
        models[[i]] <- do.call(model, args)
    }
    all_info <- list(
        formulas=formulas,
        models=models
    )
    class(all_info) <- paste0('block_', model)
    return(all_info)
}


#' Summarizing block regression.
#' 
#' \code{summary} method for class "\code{block_lm}".
#' 
#' @param model An object of class "\code{block_lm}", usually, a result of a
#'   call to \code{\link{build_model}}.
#' @param ... Further arguments passed to or from other methods.
#' @return The function computes and returns a named list of summary statistics
#'   of the fitted linear models given in \code{model}. The list has the
#'   following elements:
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
#' @seealso \code{\link{build_model}}, \code{\link{print.block_lm_summary}}
#' @examples
#' # 2 blocks: Petal.Length; Petal.Length + Petal.Width
#' model1 <- build_model(Sepal.Length, Petal.Length, Petal.Width, data=iris, model='lm')
#' summary(model1)
#' coef(model1)
#' 
#' # 2 blocks: Species; Species + Petal.Length + Petal.Width + Petal.Length:Petal.Width
#' model2 <- build_model(Sepal.Length, Species, c(Petal.Length * Petal.Width), data=iris, model='lm')
#' summary(model2)
#' coef(model2)
#' @export
summary.block_lm <- function(model, ...) {
    obj <- list()
    obj$formulas <- model$formulas
    
    model_names <- paste('Model', 1:length(model$models))
    delta_anova <- do.call(anova, model$models) 
    
    resids <- list()
    coefs <- list()
    r_sq <- c()
    overall <- data.frame(
        r.sq=numeric(0),
        adj.r.sq=numeric(0),
        F=numeric(0),
        df1=numeric(0),
        df2=numeric(0),
        p=numeric(0),
        delta.r.sq=numeric(0),
        delta.F=numeric(0),
        delta.p=numeric(0))
    
    # pull out residuals, coefficients, and R-squared and F
    for (m in 1:length(model$models)) {
        # calculate residuals for model
        resids[[m]] <- quantile(residuals(model$models[[m]]))
        
        summ <- summary(model$models[[m]])
        coefs[[m]] <- coef(summ)
        
        if (nrow(coefs[[m]]) > 1) {
            f_data <- summ$fstatistic
            r_sq[m] <- summ$r.squared
            
            if (m == 1) {
                delta_r_sq <- NA
            } else {
                delta_r_sq <- summ$r.squared - r_sq[m-1]
            }
            
            overall[m, ] <- c(
                summ$r.squared,
                summ$adj.r.squared,
                f_data[1],
                f_data[2],
                f_data[3],
                pf(f_data[1], f_data[2], f_data[3], lower.tail=FALSE),
                delta_r_sq,
                delta_anova[m, 'F'],
                delta_anova[m, 'Pr(>F)']
            )
        } else {
            # only one coefficient signals an intercept-only model, which means
            # F and R-squared aren't calculated
            overall[m, ] <- NA
        }
    }
    
    # reorganize residual information
    resids <- matrix(unlist(resids), ncol=5, byrow=TRUE)
    colnames(resids) <- c('Min', '1Q', 'Median', '3Q', 'Max')
    rownames(resids) <- model_names
    obj$residuals <- resids  
    
    colnames(overall) <- c('R Squared', 'Adj. R Squared', 'F', 'df1', 'df2',
                           'p', 'Delta R Sq.', 'Delta F', 'Delta p')
    rownames(overall) <- model_names
    
    obj$coefficients <- coefs
    obj$overall <- overall
    class(obj) <- 'block_lm_summary'
    
    return(obj)
}


#' Summarizing block regression.
#' 
#' \code{print} method for class "\code{block_lm_summary}".
#' 
#' @param model An object of class "\code{block_lm_summary}", usually, a result
#'   of a call to \code{\link{summary.block_lm}}.
#' @param digits The number of significant digits to use when printing.
#' @param signif.stars Logical. If \code{TRUE}, 'significance stars' are printed
#'   for each coefficient.
#' @param ... Further arguments passed to or from other methods.
#' @seealso \code{\link{build_model}}, \code{\link{summary.block_lm}}
#' @examples
#' # 2 blocks: Petal.Length; Petal.Length + Petal.Width
#' model1 <- build_model(Sepal.Length, Petal.Length, Petal.Width, data=iris, model='lm')
#' summary(model1)
#' coef(model1)
#' 
#' # 2 blocks: Species; Species + Petal.Length + Petal.Width + Petal.Length:Petal.Width
#' model2 <- build_model(Sepal.Length, Species, c(Petal.Length * Petal.Width), data=iris, model='lm')
#' summary(model2)
#' coef(model2)
#' @export
print.block_lm_summary <- function(
    model,
    digits=max(3L, getOption('digits') - 3L),
    signif.stars=getOption('show.signif.stars'),
    ...) {
    
    writeLines('Residuals:')
    print(model$residuals, digits=digits, ...)
    
    writeLines('\nCoefficients:')
    
    num_models <- length(model$coefficients)
    for (m in 1:num_models) {
        writeLines(paste0('lm(formula = ', format(model$formulas[[m]]), ')'))
        coefs <- capture.output(printCoefmat(model$coefficients[[m]],
                digits=digits,
                signif.stars=signif.stars,
                na.print='NA', ...))
        
        # cut off significance codes for all but last model
        if (m == num_models || !grepl('Signif. codes', coefs[length(coefs)])) {
            writeLines(coefs)
        } else {
            writeLines(coefs[1:(length(coefs)-2)])
        }
        writeLines('')
    }
    
    if (num_models > 1 || !is.na(model$overall[1, 'R Squared'])) {
        # if statement covers intercept-only case
        
        writeLines('Overall:')
        
        # add a vertical line between R-squared and delta R-squared
        overall <- cbind(
            model$overall[, 1:6],
            rep('|', nrow(model$overall)),
            model$overall[, 7:9])
        colnames(overall)[7] <- '|'
        
        print(overall, digits=digits)
    }
    
    invisible(model)
}


#' Extract model coefficients.
#' 
#' \code{coef} method for class "\code{block_lm}".
#' 
#' @param model An object of class "\code{block_lm}", usually, a result of a
#'   call to \code{\link{build_model}}.
#' @param num Numeric vector with the index of model(s) from which to return the
#'   coefficients.
#' @param ... Further arguments passed to or from other methods.
#' @return The coefficients of block(s) `num`, or if `num` is NULL, a list of
#'   coefficients from all blocks.
#' @seealso \code{\link{build_model}}, \code{coef.block_lm_summary},
#'   \code{\link{fitted.block_lm}}, \code{residuals.block_lm}
#' @examples
#' # 2 blocks: Petal.Length; Petal.Length + Petal.Width
#' model1 <- build_model(Sepal.Length, Petal.Length, Petal.Width, data=iris, model='lm')
#' summary(model1)
#' coef(model1)  # returns both blocks 1 and 2
#' 
#' # 2 blocks: Species; Species + Petal.Length + Petal.Width + Petal.Length:Petal.Width
#' model2 <- build_model(Sepal.Length, Species, c(Petal.Length * Petal.Width), data=iris, model='lm')
#' summary(model2)
#' coef(model2, num=2)  # returns second block
#' @export
coef.block_lm <- function(model, num=NULL, ...) {
    if (!is.null(num)) {
        if (length(num) > 1) {
            return(lapply(model$models[num], coef, ...))
        } else {
            return(coef(model$models[[num]], ...))
        }
    } else {
        return(lapply(model$models, coef, ...))
    }
}


#' Extract model coefficients.
#' 
#' \code{coef} method for class "\code{block_lm_summary}".
#' 
#' @param model An object of class "\code{block_lm_summary}", usually, a result
#'   of a call to \code{\link{block_lm_summary}}.
#' @param num Numeric vector with the index of model(s) from which to return the
#'   coefficients.
#' @param ... Further arguments passed to or from other methods.
#' @return The coefficients of block(s) `num`, or if `num` is NULL, a list of
#'   coefficients from all blocks.
#' @seealso \code{\link{summary.block_lm}}, \code{coef.block_lm},
#'   \code{residuals.block_lm_summary}
#' @examples
#' # 2 blocks: Petal.Length; Petal.Length + Petal.Width
#' model1 <- build_model(Sepal.Length, Petal.Length, Petal.Width, data=iris, model='lm')
#' coef(summary(model1))  # returns both blocks 1 and 2
#' 
#' # 2 blocks: Species; Species + Petal.Length + Petal.Width + Petal.Length:Petal.Width
#' model2 <- build_model(Sepal.Length, Species, c(Petal.Length * Petal.Width), data=iris, model='lm')
#' coef(summary(model2), num=2)  # returns second block
#' @export
coef.block_lm_summary <- function(model, num=NULL, ...) {
    if (!is.null(num)) {
        if (length(num) > 1) {
            return(model$coefficients[num])
        } else {
            return(model$coefficients[[num]])
        }
    } else {
        return(model$coefficients)
    }
}


#' Extract model residuals.
#' 
#' \code{residuals} method for class "\code{block_lm}".
#' 
#' @param model An object of class "\code{block_lm}", usually, a result of a
#'   call to \code{\link{build_model}}.
#' @param num Numeric vector with the index of model(s) from which to return the
#'   residuals.
#' @param ... Further arguments passed to or from other methods.
#' @return The residuals of block(s) `num`, or if `num` is NULL, a list of
#'   residuals from all blocks.
#' @seealso \code{\link{build_model}}, \code{residuals.block_lm_summary},
#'   \code{\link{fitted.block_lm}}, \code{coef.block_lm}
#' @examples
#' # 2 blocks: Petal.Length; Petal.Length + Petal.Width
#' model1 <- build_model(Sepal.Length, Petal.Length, Petal.Width, data=iris, model='lm')
#' summary(model1)
#' residuals(model1)  # returns both blocks 1 and 2
#' 
#' # 2 blocks: Species; Species + Petal.Length + Petal.Width + Petal.Length:Petal.Width
#' model2 <- build_model(Sepal.Length, Species, c(Petal.Length * Petal.Width), data=iris, model='lm')
#' summary(model2)
#' residuals(model2, num=2)  # returns second block
#' @export
residuals.block_lm <- function(model, num=NULL, ...) {
    if (!is.null(num)) {
        if (length(num) > 1) {
            return(lapply(model$models[num], residuals, ...))
        } else {
            return(residuals(model$models[[num]], ...))
        }
    } else {
        return(lapply(model$models, residuals, ...))
    }
}


#' Extract model residuals.
#' 
#' \code{residuals} method for class "\code{block_lm_summary}".
#' 
#' @param model An object of class "\code{block_lm_summary}", usually, a result
#'   of a call to \code{\link{block_lm_summary}}.
#' @param num Numeric vector with the index of model(s) from which to return the
#'   residuals.
#' @param ... Further arguments passed to or from other methods.
#' @return The residuals of block(s) `num`, or if `num` is NULL, a list of
#'   residuals from all blocks.
#' @seealso \code{\link{summary.block_lm}}, \code{residuals.block_lm},
#'   \code{coef.block_lm_summary}
#' @examples
#' # 2 blocks: Petal.Length; Petal.Length + Petal.Width
#' model1 <- build_model(Sepal.Length, Petal.Length, Petal.Width, data=iris, model='lm')
#' residuals(summary(model1))  # returns both blocks 1 and 2
#' 
#' # 2 blocks: Species; Species + Petal.Length + Petal.Width + Petal.Length:Petal.Width
#' model2 <- build_model(Sepal.Length, Species, c(Petal.Length * Petal.Width), data=iris, model='lm')
#' residuals(summary(model2), num=2)  # returns second block
#' @export
residuals.block_lm_summary <- function(model, num=NULL, ...) {
    if (!is.null(num)) {
        return(model$residuals[num, ])
    } else {
        return(model$residuals)
    }
}


#' Extract model fitted values.
#' 
#' \code{fitted} method for class "\code{block_lm}".
#' 
#' @param model An object of class "\code{block_lm}", usually, a result of a
#'   call to \code{\link{build_model}}.
#' @param num Numeric vector with the index of model(s) from which to return the
#'   fitted values.
#' @param ... Further arguments passed to or from other methods.
#' @return The fitted values of block(s) `num`, or if `num` is NULL, a list of
#'   fitted values from all blocks.
#' @seealso \code{\link{build_model}}, \code{coef.block_lm},
#'   \code{residuals.block_lm}
#' @examples
#' # 2 blocks: Petal.Length; Petal.Length + Petal.Width
#' model1 <- build_model(Sepal.Length, Petal.Length, Petal.Width, data=iris, model='lm')
#' summary(model1)
#' fitted(model1)  # returns both blocks 1 and 2
#' 
#' # 2 blocks: Species; Species + Petal.Length + Petal.Width + Petal.Length:Petal.Width
#' model2 <- build_model(Sepal.Length, Species, c(Petal.Length * Petal.Width), data=iris, model='lm')
#' summary(model2)
#' fitted(model2, num=2)  # returns second block
#' @export
fitted.block_lm <- function(model, num=NULL, ...) {
    if (!is.null(num)) {
        if (length(num) > 1) {
            return(lapply(model$models[num], fitted, ...))
        } else {
            return(fitted(model$models[[num]], ...))
        }
    } else {
        return(lapply(model$models, fitted, ...))
    }
}




