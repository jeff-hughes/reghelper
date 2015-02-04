#' Incremental block modelling.
#' 
#' \code{block_lm} allows you to incrementally add terms to a linear regression
#' model.
#' 
#' Given a list of names of variables at each step, this function will run a
#' series of models, adding the terms for each block incrementally.
#' 
#' @param dv A string of the variable name to be used as the dependent variable.
#' @param blocks A list of character vectors, one for each block, with the names
#'   of variables (or interaction terms) to add for each block. Variables from
#'   previous blocks will be added to each subsequent block.
#' @param data An optional data frame containing the variables in the model. If
#'   not found in \code{data}, the variables are taken from the environment from
#'   which the function is called.
#' @return A named list with the following elements:
#' \tabular{ll}{
#'   \code{formulas} \tab A list of the regression formulas used for each block.
#'   \cr
#'   \code{models} \tab A list of all regression models.\cr
#' }
#' @examples TODO: Need to complete.
#' @export
block_lm <- function(dv, blocks, data=NULL) {
    formulas <- list()
    models <- list()
    for (i in 1:length(blocks)) {
        predictors <- ''
        for (j in 1:i) {  # include all previous blocks
            if (j == 1) {
                predictors <- paste(blocks[[j]], collapse=' + ')
            } else {
                predictors <- paste0(
                    predictors,
                    ' + ',
                    paste(blocks[[j]], collapse=' + ')
                )
            }
        }
        formulas[[i]] <- paste0(dv, ' ~ ', predictors)
        models[[i]] <- lm(formula=formulas[[i]], data)
    }
    all_info <- list(
        formulas=formulas,
        models=models
    )
    class(all_info) <- 'block_lm'
    return(all_info)
}


#' Summarizing block regression.
#' 
#' \code{summary} method for class "\code{block_lm}".
#' 
#' @param model An object of class "\code{block_lm}", usually, a result of a
#'   call to \code{\link{block_lm}}.
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
#' @seealso \code{\link{block_lm}}, \code{\link{print.block_lm_summary}}
#' @examples TODO: Need to complete.
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
#' @seealso \code{\link{block_lm}}, \code{\link{summary.block_lm}}
#' @examples TODO: Need to complete.
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
        writeLines(paste0('lm(formula = ', model$formulas[[m]], ')'))
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
    
    writeLines('Overall:')
    
    # add a vertical line between R-squared and delta R-squared
    overall <- cbind(
        model$overall[, 1:6],
        rep('|', nrow(model$overall)),
        model$overall[, 7:9])
    colnames(overall)[7] <- '|'
    
    print(overall, digits=digits)
    
    invisible(model)
}




