#' Incremental block modelling.
#' 
#' \code{build_model} allows you to incrementally add terms to a linear
#' regression model. Given a list of names of variables at each step, this
#' function will run a series of models, adding the terms for each block
#' incrementally to "build up" to a final model including all the terms.
#' 
#' \strong{Note:} Cases with missing data are dropped based on the \emph{final}
#' model that includes all the relevant terms. This ensures that all the models
#' are tested on the same number of cases.
#' 
#' @param dv The variable name to be used as the dependent variable.
#' @param ... Pass through variable names (or interaction terms) to add for each
#'   block. To add one term to a block, just pass it through directly; to
#'   add multiple terms, pass it through in a vector or list. Blocks will be
#'   added in the order they are passed to the function, and variables from
#'   previous blocks will be included with each subsequent block, so they do not
#'   need to be repeated.
#' @param data An optional data frame containing the variables in the model. If
#'   not found in \code{data}, the variables are taken from the environment from
#'   which the function is called.
#' @param opts List of arguments to be passed to the model function.
#' @param model The type of model to use; supports 'lm', 'aov', and 'glm'.
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
#' regression model. Given a list of names of variables at each step, this
#' function will run a series of models, adding the terms for each block
#' incrementally to "build up" to a final model including all the terms.
#' 
#' Note that in most cases it is easier to use \code{\link{build_model}} and
#' pass variable names in directly instead of strings of variable names.
#' \code{build_model_q} uses standard evaluation in cases where such evaluation
#' is easier.
#' 
#' \strong{Note:} Cases with missing data are dropped based on the \emph{final}
#' model that includes all the relevant terms. This ensures that all the models
#' are tested on the same number of cases.
#' 
#' @param dv String of the variable name to be used as the dependent variable.
#' @param blocks List of variable names (or interaction terms) to add for each
#'   block. Each list element should be a single string with terms for that
#'   block. Variables from previous blocks will be included with each subsequent
#'   block, so they do not need to be repeated.
#' @param data An optional data frame containing the variables in the model. If
#'   not found in \code{data}, the variables are taken from the environment from
#'   which the function is called.
#' @param opts List of arguments to be passed to the model function.
#' @param model The type of model to use; supports 'lm', 'aov', and 'glm'.
#' @return A named list with the following elements:
#' \tabular{ll}{
#'   \code{formulas} \tab A list of the regression formulas used for each block.
#'   \cr
#'   \code{models} \tab A list of all regression models.\cr
#' }
#' @seealso \code{\link{build_model}}
#' @examples
#' # 2 blocks: Petal.Length; Petal.Length + Petal.Width
#' model1 <- build_model_q('Sepal.Length', list('Petal.Length + Petal.Width'),
#'     data=iris, model='lm')
#' summary(model1)
#' coef(model1)
#' 
#' # 2 blocks: Species; Species + Petal.Length + Petal.Width + Petal.Length:Petal.Width
#' model2 <- build_model_q('Sepal.Length', list('Species', 'Species + Petal.Length * Petal.Width'),
#'     data=iris, model='lm')
#' summary(model2)
#' coef(model2)
#' @export
build_model_q <- function(dv, blocks=NULL, data=NULL, opts=NULL, model='lm') {
    formulas <- list()
    models <- list()
    
    for (i in 1:length(blocks)) {
        formulas[[i]] <- as.formula(paste(dv, '~', blocks[[i]]))
    }
    
    allterms <- as.character(attr(terms(formulas[[length(formulas)]]),
        'variables'))[-1]
    
    # omit missing data so that all models are built on same number of rows
    if (!is.null(data)) {
        data <- na.omit(data[, allterms, drop=FALSE])
    } else {
        tempdata <- as.data.frame(mget(allterms))
        data <- na.omit(tempdata)
    }
    
    for (i in 1:length(blocks)) {
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


#' @rdname block_model_summ
#' @export
summary.block_lm <- function(object, ...) {
    model <- object
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
            
            # the anova() function works differently for one model than it does
            # for multiple models; if there is only one model, just set it to NA
            if (length(model$models) == 1) {
                delta_F <- NA
                delta_p <- NA
            } else {
                delta_F <- delta_anova[m, 'F']
                delta_p <- delta_anova[m, 'Pr(>F)']
            }
            
            overall[m, ] <- c(
                summ$r.squared,
                summ$adj.r.squared,
                f_data[1],
                f_data[2],
                f_data[3],
                pf(f_data[1], f_data[2], f_data[3], lower.tail=FALSE),
                delta_r_sq,
                delta_F,
                delta_p
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


#' @rdname block_model_summ
#' @export
print.block_lm_summary <- function(
    x,
    digits=max(3L, getOption('digits') - 3L),
    signif.stars=getOption('show.signif.stars'),
    ...) {
    
    model <- x
    
    writeLines('Residuals:')
    print(model$residuals, digits=digits, ...)
    
    writeLines('\nCoefficients:')
    
    num_models <- length(model$coefficients)
    for (m in 1:num_models) {
        frm <- format(model$formulas[[m]])
        if (length(frm) > 1) {
            frm[1] <- paste0('lm(formula = ', frm[1])
            frm[length(frm)] <- paste0(frm[length(frm)], ')')
        } else {
            frm <- paste0('lm(formula = ', frm, ')')
        }
        writeLines(frm)
        coefs <- utils::capture.output(printCoefmat(model$coefficients[[m]],
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


#' @rdname block_model_summ
#' @export
coef.block_lm <- function(object, num=NULL, ...) {
    model <- object
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


#' @export
coef.block_lm_summary <- function(object, num=NULL, ...) {
    model <- object
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


#' @rdname block_model_summ
#' @export
residuals.block_lm <- function(object, num=NULL, ...) {
    model <- object
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


#' @export
residuals.block_lm_summary <- function(object, num=NULL, ...) {
    model <- object
    if (!is.null(num)) {
        return(model$residuals[num, ])
    } else {
        return(model$residuals)
    }
}


#' @rdname block_model_summ
#' @export
fitted.block_lm <- function(object, num=NULL, ...) {
    model <- object
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


#' @rdname block_model_summ
#' @export
summary.block_aov <- function(object, ...) {
    model <- object
    obj <- list()
    obj$formulas <- model$formulas
    
    model_names <- paste('Model', 1:length(model$models))
    
    resids <- list()
    summ <- list()
    
    # pull out residuals, coefficients, and R-squared and F
    for (m in 1:length(model$models)) {
        # calculate residuals for model
        resids[[m]] <- quantile(residuals(model$models[[m]]))
        
        summ[[m]] <- summary(model$models[[m]])
    }
    
    # reorganize residual information
    resids <- matrix(unlist(resids), ncol=5, byrow=TRUE)
    colnames(resids) <- c('Min', '1Q', 'Median', '3Q', 'Max')
    rownames(resids) <- model_names
    obj$residuals <- resids  
    
    obj$summaries <- summ
    class(obj) <- 'block_aov_summary'
    
    return(obj)
}


#' @rdname block_model_summ
#' @export
print.block_aov_summary <- function(
    x,
    digits=max(3L, getOption('digits') - 3L),
    signif.stars=getOption('show.signif.stars'),
    ...) {
    
    model <- x
    
    writeLines('Residuals:')
    print(model$residuals, digits=digits, ...)
    
    writeLines('\nTables:')
    
    num_models <- length(model$summaries)
    for (m in 1:num_models) {
        writeLines(paste0('aov(formula = ', format(model$formulas[[m]]), ')'))
        summ <- utils::capture.output(print(model$summaries[[m]],
            digits=digits,
            signif.stars=signif.stars,
            na.print='NA', ...))
        
        # cut off significance codes for all but last model
        if (m == num_models || !grepl('Signif. codes', summ[length(summ)])) {
            writeLines(summ)
        } else {
            writeLines(summ[1:(length(summ)-2)])
        }
        writeLines('')
    }
    
    invisible(model)
}


#' @rdname block_model_summ
#' @export
coef.block_aov <- function(object, num=NULL, ...) {
    model <- object
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


#' @rdname block_model_summ
#' @export
residuals.block_aov <- function(object, num=NULL, ...) {
    model <- object
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


#' @rdname block_model_summ
#' @export
fitted.block_aov <- function(object, num=NULL, ...) {
    model <- object
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


#' @rdname block_model_summ
#' @export
summary.block_glm <- function(object, ...) {
    model <- object
    obj <- list()
    obj$formulas <- model$formulas
    obj$family <- model$models[[1]]$family$family
    
    model_names <- paste('Model', 1:length(model$models))
    
    resids <- list()
    coefs <- list()
    
    # pull out residuals, coefficients, and R-squared and F
    for (m in 1:length(model$models)) {
        # calculate residuals for model
        resids[[m]] <- quantile(residuals(model$models[[m]]))
        
        summ <- summary(model$models[[m]])
        coefs[[m]] <- coef(summ)
    }
    
    # reorganize residual information
    resids <- matrix(unlist(resids), ncol=5, byrow=TRUE)
    colnames(resids) <- c('Min', '1Q', 'Median', '3Q', 'Max')
    rownames(resids) <- model_names
    obj$residuals <- resids  
    
    obj$coefficients <- coefs
    class(obj) <- 'block_glm_summary'
    
    return(obj)
}


#' @rdname block_model_summ
#' @export
print.block_glm_summary <- function(
    x,
    digits=max(3L, getOption('digits') - 3L),
    signif.stars=getOption('show.signif.stars'),
    ...) {
    
    model <- x
    
    writeLines('Deviance Residuals:')
    print(model$residuals, digits=digits, ...)
    
    writeLines('\nCoefficients:')
    
    num_models <- length(model$coefficients)
    for (m in 1:num_models) {
        writeLines(paste0('glm(formula = ', format(model$formulas[[m]]),
            ', family = "', model$family, '")'))
        coefs <- utils::capture.output(printCoefmat(model$coefficients[[m]],
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
    
    invisible(model)
}


#' @rdname block_model_summ
#' @export
coef.block_glm <- function(object, num=NULL, ...) {
    model <- object
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


#' @export
coef.block_glm_summary <- function(object, num=NULL, ...) {
    model <- object
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


#' @rdname block_model_summ
#' @export
residuals.block_glm <- function(object, num=NULL, ...) {
    model <- object
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


#' @export
residuals.block_glm_summary <- function(object, num=NULL, ...) {
    model <- object
    if (!is.null(num)) {
        return(model$residuals[num, ])
    } else {
        return(model$residuals)
    }
}


#' @rdname block_model_summ
#' @export
fitted.block_glm <- function(object, num=NULL, ...) {
    model <- object
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




