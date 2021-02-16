#' Graph interactions for fitted models.
#' 
#' \code{graph_model} provides an easy way to graph interactions in fitted
#' models (linear, generalized linear, hierarchical linear, or ANOVA). Selected
#' variables will be graphed at +/- 1 SD (if continuous) or at each level of the
#' factor (if categorical).
#' 
#' If there are additional covariates in the model other than what is indicated
#' to be graphed by the function, these variables will be plotted at their
#' respective means. In the case of a categorical covariate, the results will be
#' averaged across all its levels.
#' 
#' @param model A fitted linear model of type 'lm', 'aov', 'glm', 'lme', or
#'   'merMod'.
#' @param y The variable to be plotted on the y-axis. This variable is required
#'   for the graph.
#' @param x The variable to be plotted on the x-axis. This variable is required
#'   for the graph.
#' @param lines The variable to be plotted using separate lines (optional).
#' @param split The variable to be split among separate graphs (optional).
#' @param errorbars A string indicating what kind of error bars to show.
#'   Acceptable values are "CI" (95\% confidence intervals), "SE" (+/-1 standard
#'   error of the predicted means), or "none".
#' @param ymin Number indicating the minimum value for the y-axis scale. Default
#'   NULL value will adjust position to the lowest y value.
#' @param ymax Number indicating the maximum value for the y-axis scale. Default
#'   NULL value will adjust position to the highest y value.
#' @param labels A named list with strings for the various plot labels: 'title'
#'   will set the graph title, 'y' sets the y-axis label, 'x' sets the x-axis
#'   label, 'lines' sets the legend label, and 'split' sets the label for the 
#'   facet. If any label is not set, the names of the variables will be used.
#'   Setting a label explicitly to NA will set a label with an empty string.
#' @param bargraph Logical. TRUE will draw a bar graph of the results; FALSE
#'   will draw a line graph of the results.
#' @param draw.legend Logical. Whether or not to draw legend on the graph.
#' @param dodge A numeric value indicating the amount each point on the graph
#'   should be shifted left or right, which can help for readability when points
#'   are close together. Default value is 0, with .1 or .2 probably sufficient
#'   in most cases.
#' @param exp Logical. If TRUE, the exponential function \code{exp()} will be
#'   used to transform the y-axis (i.e., e to the power of y). Useful for
#'   logistic regressions or for converting log-transformed y-values to their
#'   original units.
#' @param type The type of prediction required. The default 'link' is on the
#'   scale of the linear predictors; the alternative 'response' is on the scale
#'   of the response variable. For more information, see
#'   \code{\link{predict.glm}}.
#' @param ... Not currently implemented; used to ensure consistency with S3 generic.
#' @return A ggplot2 graph of the plotted variables in the model.
#' @examples
#' # iris data
#' model <- lm(Sepal.Width ~ Sepal.Length * Species, data=iris)
#' graph_model(model, y=Sepal.Width, x=Sepal.Length, lines=Species)
#' 
#' # Orthodont data
#' if (require(nlme, quietly=TRUE)) {
#'     model <- lme(distance ~ age * Sex, data=Orthodont, random=~1|Subject)
#'     graph_model(model, y=distance, x=age, lines=Sex)
#' }
#' 
#' # Arabidopsis data
#' if (require(lme4, quietly=TRUE)) {
#'     model <- lmer(total.fruits ~ nutrient * amd + rack + (1|gen), data=Arabidopsis)
#'     graph_model(model, y=total.fruits, x=nutrient, lines=amd)
#' }
#' @export
graph_model <- function(model, ...) UseMethod('graph_model')


#' Graph interactions for fitted models.
#' 
#' \code{graph_model_q} provides an easy way to graph interactions in fitted
#' models (linear, generalized linear, hierarchical linear, or ANOVA). Selected
#' variables will be graphed at +/- 1 SD (if continuous) or at each level of the
#' factor (if categorical).
#' 
#' If there are additional covariates in the model other than what is indicated
#' to be graphed by the function, these variables will be plotted at their
#' respective means. In the case of a categorical covariate, the results will be
#' averaged across all its levels.
#' 
#' Note that in most cases it is easier to use \code{\link{graph_model}} and
#' pass variable names in directly instead of strings of variable names.
#' \code{graph_model_q} uses standard evaluation in cases where such
#' evaluation is easier.
#' 
#' @param model A fitted linear model of type 'lm', 'aov', 'glm', 'lme', or
#'   'merMod'.
#' @param y The variable to be plotted on the y-axis. This variable is required
#'   for the graph.
#' @param x The variable to be plotted on the x-axis. This variable is required
#'   for the graph.
#' @param lines The variable to be plotted using separate lines (optional).
#' @param split The variable to be split among separate graphs (optional).
#' @param errorbars A string indicating what kind of error bars to show.
#'   Acceptable values are "CI" (95\% confidence intervals), "SE" (+/-1 standard
#'   error of the predicted means), or "none".
#' @param ymin Number indicating the minimum value for the y-axis scale. Default
#'   NULL value will adjust position to the lowest y value.
#' @param ymax Number indicating the maximum value for the y-axis scale. Default
#'   NULL value will adjust position to the highest y value.
#' @param labels A named list with strings for the various plot labels: 'title'
#'   will set the graph title, 'y' sets the y-axis label, 'x' sets the x-axis
#'   label, 'lines' sets the legend label, and 'split' sets the label for the 
#'   facet. If any label is not set, the names of the variables will be used.
#'   Setting a label explicitly to NA will set a label with an empty string.
#' @param bargraph Logical. TRUE will draw a bar graph of the results; FALSE
#'   will draw a line graph of the results.
#' @param draw.legend Logical. Whether or not to draw legend on the graph.
#' @param dodge A numeric value indicating the amount each point on the graph
#'   should be shifted left or right, which can help for readability when points
#'   are close together. Default value is 0, with .1 or .2 probably sufficient
#'   in most cases.
#' @param exp Logical. If TRUE, the exponential function \code{exp()} will be
#'   used to transform the y-axis (i.e., e to the power of y). Useful for
#'   logistic regressions or for converting log-transformed y-values to their
#'   original units.
#' @param type The type of prediction required. The default 'link' is on the
#'   scale of the linear predictors; the alternative 'response' is on the scale
#'   of the response variable. For more information, see
#'   \code{\link{predict.glm}}.
#' @param ... Not currently implemented; used to ensure consistency with S3 generic.
#' @return A ggplot2 graph of the plotted variables in the model.
#' @seealso \code{\link{graph_model}}
#' @examples
#' # iris data
#' model <- lm(Sepal.Width ~ Sepal.Length * Species, data=iris)
#' graph_model_q(model, y='Sepal.Width', x='Sepal.Length', lines='Species')
#' 
#' # Orthodont data
#' if (require(nlme, quietly=TRUE)) {
#'     model <- lme(distance ~ age * Sex, data=Orthodont, random=~1|Subject)
#'     graph_model_q(model, y='distance', x='age', lines='Sex')
#' }
#' 
#' # Arabidopsis data
#' if (require(lme4, quietly=TRUE)) {
#'     model <- lmer(total.fruits ~ nutrient * amd + rack + (1|gen), data=Arabidopsis)
#'     graph_model_q(model, y='total.fruits', x='nutrient', lines='amd')
#' }
#' @export
graph_model_q <- function(model, ...) UseMethod('graph_model_q')


#' @describeIn graph_model Graphing linear models.
#' @export
graph_model.lm <- function(model, y, x, lines=NULL, split=NULL,
    errorbars=c('CI', 'SE', 'none'), ymin=NULL, ymax=NULL, labels=NULL,
    bargraph=FALSE, draw.legend=TRUE, dodge=0, exp=FALSE, ...) {
    
    call <- as.list(match.call())[-1]
    
    # convert variable names to strings
    call$y <- deparse(substitute(y))
    call$x <- deparse(substitute(x))
    if (!is.null(call$lines)) {
        call$lines <- deparse(substitute(lines))
    }
    if (!is.null(call$split)) {
        call$split <- deparse(substitute(split))
    }
    
    return(do.call(graph_model_q.lm, call))
}


#' @describeIn graph_model_q Graphing linear models.
#' @export
graph_model_q.lm <- function(model, y, x, lines=NULL, split=NULL,
    errorbars=c('CI', 'SE', 'none'), ymin=NULL, ymax=NULL, labels=NULL,
    bargraph=FALSE, draw.legend=TRUE, dodge=0, exp=FALSE, ...) {
    
    call <- as.list(match.call())[-1]
    call$type <- 'response'
    
    return(do.call(graph_model_q.glm, call))
}


#' @describeIn graph_model Graphing ANOVA.
#' @export
graph_model.aov <- function(model, y, x, lines=NULL, split=NULL,
    errorbars=c('CI', 'SE', 'none'), ymin=NULL, ymax=NULL, labels=NULL,
    bargraph=FALSE, draw.legend=TRUE, dodge=0, exp=FALSE, ...) {
    
    call <- as.list(match.call())[-1]
    
    # convert variable names to strings
    call$y <- deparse(substitute(y))
    call$x <- deparse(substitute(x))
    if (!is.null(call$lines)) {
        call$lines <- deparse(substitute(lines))
    }
    if (!is.null(call$split)) {
        call$split <- deparse(substitute(split))
    }
    
    return(do.call(graph_model_q.aov, call))
}


#' @describeIn graph_model_q Graphing ANOVA.
#' @export
graph_model_q.aov <- function(model, y, x, lines=NULL, split=NULL,
    errorbars=c('CI', 'SE', 'none'), ymin=NULL, ymax=NULL, labels=NULL,
    bargraph=FALSE, draw.legend=TRUE, dodge=0, exp=FALSE, ...) {
    
    call <- as.list(match.call())[-1]
    
    return(do.call(graph_model_q.lm, call))
}


#' @describeIn graph_model Graphing generalized linear models.
#' @export
graph_model.glm <- function(model, y, x, lines=NULL, split=NULL,
    type=c('link', 'response'), errorbars=c('CI', 'SE', 'none'), ymin=NULL,
    ymax=NULL, labels=NULL, bargraph=FALSE, draw.legend=TRUE, dodge=0,
    exp=FALSE, ...) {
    
    call <- as.list(match.call())[-1]
    
    # convert variable names to strings
    call$y <- deparse(substitute(y))
    call$x <- deparse(substitute(x))
    if (!is.null(call$lines)) {
        call$lines <- deparse(substitute(lines))
    }
    if (!is.null(call$split)) {
        call$split <- deparse(substitute(split))
    }
    
    return(do.call(graph_model_q.glm, call))
}


#' @describeIn graph_model_q Graphing generalized linear models.
#' @export
graph_model_q.glm <- function(model, y, x, lines=NULL, split=NULL,
    type=c('link', 'response'), errorbars=c('CI', 'SE', 'none'), ymin=NULL,
    ymax=NULL, labels=NULL, bargraph=FALSE, draw.legend=TRUE, dodge=0,
    exp=FALSE, ...) {
    
    type <- match.arg(type)
    errorbars <- match.arg(errorbars)
    
    data <- model$model
    factors <- list()
    
    # determine whether each variable is categorical (factor) or continuous, and
    # set up the points at which to graph each variable, to be used later
    i <- 1
    for (term in c(x, lines, split)) {
        if (!is.null(term)) {
            if (is.factor(data[[term]])) {
                factors[[i]] <- levels(data[[term]])
            } else if (is.character(data[[term]])) {
                data[[term]] <- factor(data[[term]])
                factors[[i]] <- levels(data[[term]])
            } else {
                factors[[i]] <- c(
                    mean(data[[term]], na.rm=TRUE)+sd(data[[term]], na.rm=TRUE),
                    mean(data[[term]], na.rm=TRUE)-sd(data[[term]], na.rm=TRUE)
                )
            }
            i <- i + 1
        }
    }
    
    # set up grid of points to graph the model at, to predict cell means
    if (is.null(lines) && is.null(split)) {
        grid <- with(data, expand.grid(
            x=factors[[1]],
            g=1  # dummy 'group' so ggplot still draws lines
        ))
        names(grid)[names(grid) == 'x'] <- x
    } else if (is.null(split)) {
        grid <- with(data, expand.grid(
            x=factors[[1]],
            lines=factors[[2]]
        ))
        names(grid) <- c(x, lines)
    } else {
        grid <- with(data, expand.grid(
            x=factors[[1]],
            lines=factors[[2]],
            split=factors[[3]]
        ))
        names(grid) <- c(x, lines, split)
    }
    
    # add in other variables that are in the model, but are not selected to be
    # graphed; these variables get plotted at their means
    variables <- all.vars(formula(model))[-1]
    factor_name <- NULL
    for (i in 1:length(variables)) {
        if (!(variables[[i]] %in% colnames(grid))) {
            v <- data[[variables[[i]]]]
            if (is.character(v)) {
                data[[variables[[i]]]] <- factor(v)
            }
            
            if (is.factor(v)) {
                # if factor, must include all levels in model for
                # predict() to work properly
                temp_list <- lapply(as.list(grid), unique)
                    # get unique values and put into list format
                temp_list[[variables[[i]]]] <- levels(v)
                grid <- expand.grid(temp_list)
                factor_name <- variables[[i]]
            } else {
                grid[[variables[[i]]]] <- mean(v, na.rm=TRUE)  # if continuous, include in model at mean
            }
        }
    }
    
    # predict cell means
    predicted <- predict(model, newdata=grid, type=type, se.fit=TRUE)
    if (exp == TRUE) {
        grid[[y]] <- exp(predicted$fit)
    } else {
        grid[[y]] <- predicted$fit
    }
    
    # add error bars, if desired
    errors <- FALSE
    if (errorbars == 'CI') {
        grid$error_upper <- predicted$fit + 1.96 * predicted$se.fit
        grid$error_lower <- predicted$fit - 1.96 * predicted$se.fit
        errors <- TRUE
    } else if (errorbars == 'SE') {
        grid$error_upper <- predicted$fit + predicted$se.fit
        grid$error_lower <- predicted$fit - predicted$se.fit
        errors <- TRUE
    }
    if (exp == TRUE && errors == TRUE) {
        grid$error_upper <- exp(grid$error_upper)
        grid$error_lower <- exp(grid$error_lower)
    }
    
    # workaround to include factor covariates -- average across levels;
    # only works for one factor covariate
    if (!is.null(factor_name)) {
        grid2 <- subset(grid, FALSE)
        lev <- levels(data[[factor_name]])
        location <- which(names(grid) == factor_name)
        for (i in 1:(nrow(grid)/length(lev))) {
            grid2[i, -location] <- apply(grid[seq(i, nrow(grid),
                by=(nrow(grid)/length(lev))), -location], 2, mean, na.rm=TRUE)
        }
        grid2[[location]] <- NULL
        grid <- grid2
    }
    
    # factor all the variables, to make for a cleaner graph
    for (term in c(x, lines, split)) {
        if (!is.null(term) && !is.factor(data[[term]])) {
            grid[[term]] <- factor(grid[[term]], labels=c('-1 SD', '+1 SD'))
        }
    }
    
    # add in label for the 'split' variable, if one exists
    if (!is.null(split)) {
        if (is.null(labels$split)) {
            labels$split <- paste0(split, ': ')
        } else if (is.na(labels$split)) {
            labels$split <- ''
        } else {
            labels$split <- paste0(labels$split, ': ')
        }
        
        if (!is.factor(data[[split]])) {
            levels(grid[[split]]) <- c(paste0(labels$split, '-1 SD'),
                paste0(labels$split, '+1 SD'))
        } else {
            num_levels <- length(levels(grid[[split]]))
            levels(grid[[split]]) <- paste0(rep(labels$split, num_levels),
                levels(grid[[split]]))
        }
    }
    
    if (is.null(lines)) {
        lines <- 'g'  # dummy group so ggplot still draws lines
        draw.legend <- FALSE
    }
    
    # build and return graph
    graph <- .build_plot(grid, y, x, lines, split, errors, ymin, ymax,
        labels, bargraph, draw.legend, dodge, exp)
    return(graph)
}


#' @describeIn graph_model Graphing hierarchical linear models (nlme).
#' @export
graph_model.lme <- function(model, y, x, lines=NULL, split=NULL,
    errorbars=c('CI', 'SE', 'none'), ymin=NULL, ymax=NULL, labels=NULL,
    bargraph=FALSE, draw.legend=TRUE, dodge=0, exp=FALSE, ...) {
    
    call <- as.list(match.call())[-1]
    
    # convert variable names to strings
    call$y <- deparse(substitute(y))
    call$x <- deparse(substitute(x))
    if (!is.null(call$lines)) {
        call$lines <- deparse(substitute(lines))
    }
    if (!is.null(call$split)) {
        call$split <- deparse(substitute(split))
    }
    
    return(do.call(graph_model_q.lme, call))
}


#' @describeIn graph_model_q Graphing hierarchical linear models (nlme).
#' @export
graph_model_q.lme <- function(model, y, x, lines=NULL, split=NULL,
    errorbars=c('CI', 'SE', 'none'), ymin=NULL, ymax=NULL, labels=NULL,
    bargraph=FALSE, draw.legend=TRUE, dodge=0, exp=FALSE, ...) {
    
    errorbars <- match.arg(errorbars)
    
    data <- model$data
    factors <- list()
    
    # determine whether each variable is categorical (factor) or continuous, and
    # set up the points at which to graph each variable, to be used later
    i <- 1
    for (term in c(x, lines, split)) {
        if (!is.null(term)) {
            if (is.factor(data[[term]])) {
                factors[[i]] <- levels(data[[term]])
            } else if (is.character(data[[term]])) {
                data[[term]] <- factor(data[[term]])
                factors[[i]] <- levels(data[[term]])
            } else {
                factors[[i]] <- c(
                    mean(data[[term]], na.rm=TRUE)+sd(data[[term]], na.rm=TRUE),
                    mean(data[[term]], na.rm=TRUE)-sd(data[[term]], na.rm=TRUE)
                )
            }
            i <- i + 1
        }
    }
    
    # set up grid of points to graph the model at, to predict cell means
    if (is.null(lines) && is.null(split)) {
        grid <- with(data, expand.grid(
            x=factors[[1]],
            g=1  # dummy 'group' so ggplot still draws lines
        ))
        names(grid)[names(grid) == 'x'] <- x
    } else if (is.null(split)) {
        grid <- with(data, expand.grid(
            x=factors[[1]],
            lines=factors[[2]]
        ))
        names(grid) <- c(x, lines)
    } else {
        grid <- with(data, expand.grid(
            x=factors[[1]],
            lines=factors[[2]],
            split=factors[[3]]
        ))
        names(grid) <- c(x, lines, split)
    }
    
    # add in other variables that are in the model, but are not selected to be
    # graphed; these variables get plotted at their means
    variables <- all.vars(formula(model))[-1]
    factor_name <- NULL
    for (i in 1:length(variables)) {
        if (!(variables[[i]] %in% colnames(grid))) {
            v <- data[[variables[[i]]]]
            if (is.character(v)) {
                data[[variables[[i]]]] <- factor(v)
            }

            if (is.factor(v)) {
                # if factor, must include all levels in model for
                # predict() to work properly
                temp_list <- lapply(as.list(grid), unique)
                    # get unique values and put into list format
                temp_list[[variables[[i]]]] <- levels(v)
                grid <- expand.grid(temp_list)
                factor_name <- variables[[i]]
            } else {
                grid[[variables[[i]]]] <- mean(v, na.rm=TRUE)  # if continuous, include in model at mean
            }
        }
    }
    
    # predict cell means
    predicted <- predict(model, newdata=grid, level=0)
    if (exp == TRUE) {
        grid[[y]] <- exp(predicted)
    } else {
        grid[[y]] <- predicted
    }
    
    # add error bars, if desired
    errors <- FALSE
    if (errorbars == 'CI' || errorbars == 'SE') {
        # code from: http://glmm.wikidot.com/faq
        designmat <- model.matrix(formula(model)[-2], grid)
        predvar <- diag(designmat %*% vcov(model) %*% t(designmat))
        se <- sqrt(predvar)
        
        if (errorbars == 'CI') {
            grid$error_upper <- predicted + 1.96 * se
            grid$error_lower <- predicted - 1.96 * se
        } else if (errorbars == 'SE') {
            grid$error_upper <- predicted + se
            grid$error_lower <- predicted - se
        }
        errors <- TRUE
    }
    if (exp == TRUE && errors == TRUE) {
        grid$error_upper <- exp(grid$error_upper)
        grid$error_lower <- exp(grid$error_lower)
    }
    
    # workaround to include factor covariates -- average across levels;
    # only works for one factor covariate
    if (!is.null(factor_name)) {
        grid2 <- subset(grid, FALSE)
        lev <- levels(data[[factor_name]])
        location <- which(names(grid) == factor_name)
        for (i in 1:(nrow(grid)/length(lev))) {
            grid2[i, -location] <- apply(grid[seq(i, nrow(grid),
                by=(nrow(grid)/length(lev))), -location], 2, mean, na.rm=TRUE)
        }
        grid2[[location]] <- NULL
        grid <- grid2
    }
    
    # factor all the variables, to make for a cleaner graph
    for (term in c(x, lines, split)) {
        if (!is.null(term) && !is.factor(data[[term]])) {
            grid[[term]] <- factor(grid[[term]], labels=c('-1 SD', '+1 SD'))
        }
    }
    
    # add in label for the 'split' variable, if one exists
    if (!is.null(split)) {
        if (is.null(labels$split)) {
            labels$split <- paste0(split, ': ')
        } else if (is.na(labels$split)) {
            labels$split <- ''
        } else {
            labels$split <- paste0(labels$split, ': ')
        }
        
        if (!is.factor(data[[split]])) {
            levels(grid[[split]]) <- c(paste0(labels$split, '-1 SD'),
                paste0(labels$split, '+1 SD'))
        } else {
            num_levels <- length(levels(grid[[split]]))
            levels(grid[[split]]) <- paste0(rep(labels$split, num_levels),
                levels(grid[[split]]))
        }
    }
    
    if (is.null(lines)) {
        lines <- 'g'  # dummy group so ggplot still draws lines
        draw.legend <- FALSE
    }
    
    # build and return graph
    graph <- .build_plot(grid, y, x, lines, split, errors, ymin, ymax,
        labels, bargraph, draw.legend, dodge, exp)
    return(graph)
}


#' @describeIn graph_model Graphing hierarchical linear models (lme4).
#' @export
graph_model.merMod <- function(model, y, x, lines=NULL, split=NULL,
    errorbars=c('CI', 'SE', 'none'), ymin=NULL, ymax=NULL, labels=NULL,
    bargraph=FALSE, draw.legend=TRUE, dodge=0, exp=FALSE, ...) {
    
    call <- as.list(match.call())[-1]
    
    # convert variable names to strings
    call$y <- deparse(substitute(y))
    call$x <- deparse(substitute(x))
    if (!is.null(call$lines)) {
        call$lines <- deparse(substitute(lines))
    }
    if (!is.null(call$split)) {
        call$split <- deparse(substitute(split))
    }
    
    return(do.call(graph_model_q.merMod, call))
}


#' @describeIn graph_model_q Graphing hierarchical linear models (lme4).
#' @export
graph_model_q.merMod <- function(model, y, x, lines=NULL, split=NULL,
    errorbars=c('CI', 'SE', 'none'), ymin=NULL, ymax=NULL, labels=NULL,
    bargraph=FALSE, draw.legend=TRUE, dodge=0, exp=FALSE, ...) {
    
    errorbars <- match.arg(errorbars)
    
    data <- model@frame
    factors <- list()
    
    # determine whether each variable is categorical (factor) or continuous, and
    # set up the points at which to graph each variable, to be used later
    i <- 1
    for (term in c(x, lines, split)) {
        if (!is.null(term)) {
            if (is.factor(data[[term]])) {
                factors[[i]] <- levels(data[[term]])
            } else if (is.character(data[[term]])) {
                data[[term]] <- factor(data[[term]])
                factors[[i]] <- levels(data[[term]])
            } else {
                factors[[i]] <- c(
                    mean(data[[term]], na.rm=TRUE)+sd(data[[term]], na.rm=TRUE),
                    mean(data[[term]], na.rm=TRUE)-sd(data[[term]], na.rm=TRUE)
                )
            }
            i <- i + 1
        }
    }
    
    # set up grid of points to graph the model at, to predict cell means
    if (is.null(lines) && is.null(split)) {
        grid <- with(data, expand.grid(
            x=factors[[1]],
            g=1  # dummy 'group' so ggplot still draws lines
        ))
        names(grid)[names(grid) == 'x'] <- x
    } else if (is.null(split)) {
        grid <- with(data, expand.grid(
            x=factors[[1]],
            lines=factors[[2]]
        ))
        names(grid) <- c(x, lines)
    } else {
        grid <- with(data, expand.grid(
            x=factors[[1]],
            lines=factors[[2]],
            split=factors[[3]]
        ))
        names(grid) <- c(x, lines, split)
    }
    
    # add in other variables that are in the model, but are not selected to be
    # graphed; these variables get plotted at their means
    variables <- all.vars(formula(model, fixed.only=TRUE))[-1]
    factor_name <- NULL
    for (i in 1:length(variables)) {
        if (!(variables[[i]] %in% colnames(grid))) {
            v <- data[[variables[[i]]]]
            if (is.character(v)) {
                data[[variables[[i]]]] <- factor(v)
            }
            
            if (is.factor(v)) {
                # if factor, must include all levels in model for
                # predict() to work properly
                temp_list <- lapply(as.list(grid), unique)
                    # get unique values and put into list format
                temp_list[[variables[[i]]]] <- levels(v)
                grid <- expand.grid(temp_list)
                factor_name <- variables[[i]]
            } else {
                grid[[variables[[i]]]] <- mean(v, na.rm=TRUE)  # if continuous, include in model at mean
            }
        }
    }
    
    # predict cell means
    predicted <- predict(model, newdata=grid, re.form=NA)
    if (exp == TRUE) {
        grid[[y]] <- exp(predicted)
    } else {
        grid[[y]] <- predicted
    }
    
    # add error bars, if desired
    errors <- FALSE
    if (errorbars == 'CI' || errorbars == 'SE') {
        # code from: http://glmm.wikidot.com/faq
        designmat <- model.matrix(delete.response(terms(model)), grid)
        predicted <- predict(model, grid, re.form=NA)
        predvar <- diag(designmat %*% as.matrix(vcov(model)) %*% t(designmat))
        se <- sqrt(predvar)
        
        if (errorbars == 'CI') {
            grid$error_upper <- predicted + 1.96 * se
            grid$error_lower <- predicted - 1.96 * se
        } else if (errorbars == 'SE') {
            grid$error_upper <- predicted + se
            grid$error_lower <- predicted - se
        }
        errors <- TRUE
    }
    if (exp == TRUE && errors == TRUE) {
        grid$error_upper <- exp(grid$error_upper)
        grid$error_lower <- exp(grid$error_lower)
    }
    
    # workaround to include factor covariates -- average across levels;
    # only works for one factor covariate
    if (!is.null(factor_name)) {
        grid2 <- subset(grid, FALSE)
        lev <- levels(data[[factor_name]])
        location <- which(names(grid) == factor_name)
        for (i in 1:(nrow(grid)/length(lev))) {
            grid2[i, -location] <- apply(grid[seq(i, nrow(grid),
                by=(nrow(grid)/length(lev))), -location], 2, mean, na.rm=TRUE)
        }
        grid2[[location]] <- NULL
        grid <- grid2
    }
    
    # factor all the variables, to make for a cleaner graph
    for (term in c(x, lines, split)) {
        if (!is.null(term) && !is.factor(data[[term]])) {
            grid[[term]] <- factor(grid[[term]], labels=c('-1 SD', '+1 SD'))
        }
    }
    
    # add in label for the 'split' variable, if one exists
    if (!is.null(split)) {
        if (is.null(labels$split)) {
            labels$split <- paste0(split, ': ')
        } else if (is.na(labels$split)) {
            labels$split <- ''
        } else {
            labels$split <- paste0(labels$split, ': ')
        }
        
        if (!is.factor(data[[split]])) {
            levels(grid[[split]]) <- c(paste0(labels$split, '-1 SD'),
                paste0(labels$split, '+1 SD'))
        } else {
            num_levels <- length(levels(grid[[split]]))
            levels(grid[[split]]) <- paste0(rep(labels$split, num_levels),
                levels(grid[[split]]))
        }
    }
    
    if (is.null(lines)) {
        lines <- 'g'  # dummy group so ggplot still draws lines
        draw.legend <- FALSE
    }
    
    # build and return graph
    graph <- .build_plot(grid, y, x, lines, split, errors, ymin, ymax,
        labels, bargraph, draw.legend, dodge, exp)
    return(graph)
}


#' Build ggplot object.
#' 
#' Helper function takes care of building a ggplot object, given points and
#' variables to plot.
#' 
#' @param grid Data frame with variables and points to plot.
#' @param y The variable to be plotted on the y-axis. This variable is required
#'   for the graph.
#' @param x The variable to be plotted on the x-axis. This variable is required
#'   for the graph.
#' @param lines The variable to be plotted using separate lines (optional).
#' @param split The variable to be split among separate graphs (optional).
#' @param errors Logical. Whether to plot error bars or not.
#' @param ymin Number indicating the minimum value for the y-axis scale. Default
#'   NULL value will adjust position to the lowest y value.
#' @param ymax Number indicating the maximum value for the y-axis scale. Default
#'   NULL value will adjust position to the highest y value.
#' @param labels A named list with strings for the various plot labels: 'title'
#'   will set the graph title, 'y' sets the y-axis label, 'x' sets the x-axis
#'   label, 'lines' sets the legend label, and 'split' sets the label for the 
#'   facet. If any label is not set, the names of the variables will be used.
#'   Setting a label explicitly to NA will set an empty label.
#' @param bargraph Logical. TRUE will draw a bar graph of the results; FALSE
#'   will draw a line graph of the results.
#' @param draw.legend Logical. Whether or not to draw legend on the graph.
#' @param dodge A numeric value indicating the amount each point on the graph
#'   should be shifted left or right, which can help for readability when points
#'   are close together. Default value is 0, with .1 or .2 probably sufficient
#'   in most cases.
#' @param exp Logical. If TRUE, the exponential function \code{exp()} will be
#'   used to transform the y-axis (i.e., e to the power of y). Useful for
#'   logistic regressions or for converting log-transformed y-values to their
#'   original units.
#' @return A ggplot object of the plotted variables in the model.
#' @noRd
.build_plot <- function(grid, y, x, lines=NULL, split=NULL, errors=TRUE,
    ymin=NULL, ymax=NULL, labels=NULL, bargraph=FALSE, draw.legend=TRUE,
    dodge=0, exp=FALSE) {
    
    # draw line graph
    if (bargraph == FALSE) {
        pd <- ggplot2::position_dodge(dodge)
        graph <- ggplot2::ggplot(
            data=grid,
            ggplot2::aes_string(x=x, y=y, colour=lines, ymin=ymin, ymax=ymax))
        graph <- graph + ggplot2::geom_point(position=pd) +
            ggplot2::geom_line(position=pd, ggplot2::aes_string(group=lines))
        
    # draw bar graph
    } else {
        pd <- ggplot2::position_dodge(dodge + .9) # default dodge value to
                                                  # separate bars
        graph <- ggplot2::ggplot(
            data=grid,
            ggplot2::aes_string(x=x, y=y, fill=lines, ymin=ymin, ymax=ymax))
        graph <- graph + ggplot2::geom_bar(
            position=pd,
            stat='identity',
            ggplot2::aes_string(group=lines))
    }
    
    # remove legend, if desired
    if (draw.legend == FALSE) {
        graph <- graph + ggplot2::theme(legend.position='none')
    }
    
    # add in error bars
    if (errors) {
        graph <- graph + ggplot2::geom_errorbar(
            ggplot2::aes_string(ymax='error_upper', ymin='error_lower'),
            width=0.1,
            position=pd)
    }
    
    # split graph by 'split' variable
    if (!is.null(split)) {
        graph <- graph + ggplot2::facet_grid(paste0('. ~ ', split))
    }
    
    # add labels to graph
    if (!is.null(labels)) {
        if (!is.null(labels$y) && is.na(labels$y)) {
            labels$y <- ''
        }
        if (!is.null(labels$x) && is.na(labels$x)) {
            labels$x <- ''
        }
        if (!is.null(labels$lines) && is.na(labels$lines)) {
            labels$lines <- ''
        }
        
        if (!is.null(labels$title) && !is.na(labels$title)) {
            graph <- graph + ggplot2::ggtitle(labels$title)
        }
        if (!is.null(labels$y)) {
            graph <- graph + ggplot2::ylab(labels$y)
        }
        if (!is.null(labels$x)) {
            graph <- graph + ggplot2::xlab(labels$x)
        }
        if (!is.null(labels$lines)) {
            graph <- graph + ggplot2::labs(colour=labels$lines)
        }
    }
    
    return(graph)
}




