#' Graph fitted model interactions.
#' 
#' \code{graph_model} provides an easy way to graph interactions in fitted
#' models. Selected variables will be graphed at +/- 1 SD (if continuous) or at
#' each level of the factor (if categorical).
#' 
#' @param model A fitted linear model of type 'lm' or 'glm'.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given model.
#' @return A ggplot2 graph of the plotted variables in the model.
#' @seealso \code{\link{graph_model.lm}}, \code{\link{graph_model.glm}}
#' @examples
#' # iris data
#' model <- lm(Sepal.Width ~ Sepal.Length * Species, data=iris)
#' graph_model(model, y=Sepal.Width, x=Sepal.Length, lines=Species)
#' @export
graph_model <- function(model, ...) UseMethod('graph_model')


#' Graph linear interactions.
#' 
#' \code{graph_model.lm} provides an easy way to graph interactions in linear
#' models. Selected variables will be graphed at +/- 1 SD (if continuous) or at
#' each level of the factor (if categorical).
#' 
#' If there are additional covariates in the model other than what is indicated
#' to be graphed by the function, these variables will be plotted at their
#' respective means. In the case of a categorical covariate, the results will be
#' averaged across all its levels.
#' 
#' @param model A fitted linear model of type 'lm'.
#' @param y The variable to be plotted on the y-axis. This variable is required
#'   for the graph.
#' @param x The variable to be plotted on the x-axis. This variable is required
#'   for the graph.
#' @param lines The variable to be plotted using separate lines (optional).
#' @param split The variable to be split among separate graphs (optional).
#' @param errorbars A string indicating what kind of error bars to show.
#'   Acceptable values are "CI" (95% confidence intervals), "SE" (+/-1 standard
#'   error of the predicted means), or NULL.
#' @param ymin Number indicating the minimum value for the y-axis scale. Default
#'   NULL value will adjust position to the lowest y value.
#' @param ymax Number indicating the maximum value for the y-axis scale. Default
#'   NULL value will adjust position to the highest y value.
#' @param titles: A character vector with strings for the various plot titles.
#'   In order: Graph title, 'y' title, 'x' title, 'lines' title', 'split' title.
#'   If any position is NULL, the names of the variables will be used.
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
#' @return A ggplot2 graph of the plotted variables in the model.
#' @examples
#' # iris data
#' model <- lm(Sepal.Width ~ Sepal.Length * Species, data=iris)
#' graph_model(model, y=Sepal.Width, x=Sepal.Length, lines=Species)
#' @export
graph_model.lm <- function(model, y, x, lines=NULL, split=NULL, errorbars='CI',
    ymin=NULL, ymax=NULL, titles=NULL, bargraph=FALSE, draw.legend=TRUE,
    dodge=0, exp=FALSE) {
    
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


#' Graph linear interactions.
#' 
#' \code{graph_model_q.lm} provides an easy way to graph interactions in linear
#' models. Selected variables will be graphed at +/- 1 SD (if continuous) or at
#' each level of the factor (if categorical).
#' 
#' If there are additional covariates in the model other than what is indicated
#' to be graphed by the function, these variables will be plotted at their
#' respective means. In the case of a categorical covariate, the results will be
#' averaged across all its levels.
#' 
#' Note that in most cases it is easier to use \code{\link{graph_model.lm}} and
#' pass variable names in directly instead of strings of variable names.
#' \code{graph_model_q.lm} uses standard evaluation in cases where such
#' evaluation is easier.
#' 
#' @param model A fitted linear model of type 'lm'.
#' @param y The variable to be plotted on the y-axis. This variable is required
#'   for the graph.
#' @param x The variable to be plotted on the x-axis. This variable is required
#'   for the graph.
#' @param lines The variable to be plotted using separate lines (optional).
#' @param split The variable to be split among separate graphs (optional).
#' @param errorbars A string indicating what kind of error bars to show.
#'   Acceptable values are "CI" (95% confidence intervals), "SE" (+/-1 standard
#'   error of the predicted means), or NULL.
#' @param ymin Number indicating the minimum value for the y-axis scale. Default
#'   NULL value will adjust position to the lowest y value.
#' @param ymax Number indicating the maximum value for the y-axis scale. Default
#'   NULL value will adjust position to the highest y value.
#' @param titles: A character vector with strings for the various plot titles.
#'   In order: Graph title, 'y' title, 'x' title, 'lines' title', 'split' title.
#'   If any position is NULL, the names of the variables will be used.
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
#' @seealso \code{\link{graph_model.lm}}
#' @examples
#' # iris data
#' model <- lm(Sepal.Width ~ Sepal.Length * Species, data=iris)
#' graph_model_q.lm(model, y='Sepal.Width', x='Sepal.Length', lines='Species')
#' @export
graph_model_q.lm <- function(model, y, x, lines=NULL, split=NULL,
    errorbars='CI', ymin=NULL, ymax=NULL, titles=NULL, bargraph=FALSE,
    draw.legend=TRUE, dodge=0, exp=FALSE) {
    
    call <- as.list(match.call())[-1]
    call$type <- 'response'
    
    return(do.call(graph_model_q.glm, call))
}


#' Graph linear interactions.
#' 
#' \code{graph_model.aov}  is an alias of graph_model.lm.
#' 
#' @seealso \code{\link{graph_model.lm}}, \code{\link{graph_model.glm}}
#' @export
graph_model.aov <- function(model, y, x, lines=NULL, split=NULL, errorbars='CI',
    ymin=NULL, ymax=NULL, titles=NULL, bargraph=FALSE, draw.legend=TRUE,
    dodge=0, exp=FALSE) {
    
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


#' Graph linear interactions.
#' 
#' \code{graph_model_q.aov}  is an alias of graph_model_q.lm.
#' 
#' @seealso \code{\link{graph_model_q.lm}}, \code{\link{graph_model_q.glm}}
#' @export
graph_model_q.aov <- function(model, y, x, lines=NULL, split=NULL,
    errorbars='CI', ymin=NULL, ymax=NULL, titles=NULL, bargraph=FALSE,
    draw.legend=TRUE, dodge=0, exp=FALSE) {
    
    call <- as.list(match.call())[-1]
    
    return(do.call(graph_model_q.lm, call))
}


#' Graph general linear interactions.
#' 
#' \code{graph_model.glm} provides an easy way to graph interactions in general
#' linear models. Selected variables will be graphed at +/- 1 SD (if continuous)
#' or at each level of the factor (if categorical).
#' 
#' If there are additional covariates in the model other than what is indicated
#' to be graphed by the function, these variables will be plotted at their
#' respective means. In the case of a categorical covariate, the results will be
#' averaged across all its levels.
#' 
#' @param model A fitted linear model of type 'glm'.
#' @param y The variable to be plotted on the y-axis. This variable is required
#'   for the graph.
#' @param x The variable to be plotted on the x-axis. This variable is required
#'   for the graph.
#' @param lines The variable to be plotted using separate lines (optional).
#' @param split The variable to be split among separate graphs (optional).
#' @param type The type of prediction required. The default 'link' is on the
#'   scale of the linear predictors; the alternative 'response' is on the scale
#'   of the response variable. For more information, see
#'   \code{\link{predict.glm}}.
#' @param errorbars A string indicating what kind of error bars to show.
#'   Acceptable values are "CI" (95% confidence intervals), "SE" (+/-1 standard
#'   error of the predicted means), or NULL.
#' @param ymin Number indicating the minimum value for the y-axis scale. Default
#'   NULL value will adjust position to the lowest y value.
#' @param ymax Number indicating the maximum value for the y-axis scale. Default
#'   NULL value will adjust position to the highest y value.
#' @param titles: A character vector with strings for the various plot titles.
#'   In order: Graph title, 'y' title, 'x' title, 'lines' title', 'split' title.
#'   If any position is NULL, the names of the variables will be used.
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
#' @return A ggplot2 graph of the plotted variables in the model.
#' @seealso \code{\link{graph_model.lm}}
#' @examples
#' # iris data
#' model <- lm(Sepal.Width ~ Sepal.Length * Species, data=iris)
#' graph_model(model, y=Sepal.Width, x=Sepal.Length, lines=Species)
#' @export
graph_model.glm <- function(model, y, x, lines=NULL, split=NULL,
    type=c('link', 'response'), errorbars='CI', ymin=NULL, ymax=NULL,
    titles=NULL, bargraph=FALSE, draw.legend=TRUE, dodge=0, exp=FALSE) {
    
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


#' Graph general linear interactions.
#' 
#' \code{graph_model_q.glm} provides an easy way to graph interactions in
#' general linear models. Selected variables will be graphed at +/- 1 SD (if
#' continuous) or at each level of the factor (if categorical).
#' 
#' If there are additional covariates in the model other than what is indicated
#' to be graphed by the function, these variables will be plotted at their
#' respective means. In the case of a categorical covariate, the results will be
#' averaged across all its levels.
#' 
#' Note that in most cases it is easier to use \code{\link{graph_model.glm}} and
#' pass variable names in directly instead of strings of variable names.
#' \code{graph_model_q.glm} uses standard evaluation in cases where such
#' evaluation is easier.
#' 
#' @param model A fitted linear model of type 'glm'.
#' @param y The variable to be plotted on the y-axis. This variable is required
#'   for the graph.
#' @param x The variable to be plotted on the x-axis. This variable is required
#'   for the graph.
#' @param lines The variable to be plotted using separate lines (optional).
#' @param split The variable to be split among separate graphs (optional).
#' @param type The type of prediction required. The default 'link' is on the
#'   scale of the linear predictors; the alternative 'response' is on the scale
#'   of the response variable. For more information, see
#'   \code{\link{predict.glm}}.
#' @param errorbars A string indicating what kind of error bars to show.
#'   Acceptable values are "CI" (95% confidence intervals), "SE" (+/-1 standard
#'   error of the predicted means), or NULL.
#' @param ymin Number indicating the minimum value for the y-axis scale. Default
#'   NULL value will adjust position to the lowest y value.
#' @param ymax Number indicating the maximum value for the y-axis scale. Default
#'   NULL value will adjust position to the highest y value.
#' @param titles: A character vector with strings for the various plot titles.
#'   In order: Graph title, 'y' title, 'x' title, 'lines' title', 'split' title.
#'   If any position is NULL, the names of the variables will be used.
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
#' @seealso \code{\link{graph_model.glm}}, \code{\link{graph_model_q.lm}}
#' @examples
#' # iris data
#' model <- lm(Sepal.Width ~ Sepal.Length * Species, data=iris)
#' graph_model_q.lm(model, y='Sepal.Width', x='Sepal.Length', lines='Species')
#' @export
graph_model_q.glm <- function(model, y, x, lines=NULL, split=NULL,
    type=c('link', 'response'), errorbars='CI', ymin=NULL, ymax=NULL,
    titles=NULL, bargraph=FALSE, draw.legend=TRUE, dodge=0, exp=FALSE) {
    
    type <- match.arg(type)
    
    data <- model$model
    factors <- list()
    
    # determine whether each variable is categorical (factor) or continuous, and
    # set up the points at which to graph each variable, to be used later
    i <- 1
    for (term in c(x, lines, split)) {
        if (!is.null(term)) {
            if (is.factor(data[[term]])) {
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
            if (is.factor(data[[variables[[i]]]])) {
                # if factor, must include all levels in model for
                # predict() to work properly
                temp_list <- lapply(as.list(grid), unique)
                    # get unique values and put into list format
                temp_list[[variables[[i]]]] <- levels(data[[variables[[i]]]])
                grid <- expand.grid(temp_list)
                factor_name <- variables[[i]]
            } else {
                grid[[variables[[i]]]] <- mean(data[[variables[[i]]]],
                    na.rm=TRUE)  # if continuous, include in model at mean
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
    if (errorbars == 'CI' || errorbars == 'ci') {
        grid$error_upper <- predicted$fit + 1.96 * predicted$se.fit
        grid$error_lower <- predicted$fit - 1.96 * predicted$se.fit
        errors <- TRUE
    } else if (errorbars == 'SE' || errorbars == 'se') {
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
    
    # add in title for the 'split' variable, if one exists
    if (!is.null(titles) && !is.null(split)) {
        if (!is.factor(data[[split]])) {
            levels(grid[[split]]) <- c(paste0(titles[5], ': -1 SD'),
                paste0(titles[5], ': +1 SD'))
        } else {
            num_levels <- length(levels(grid[[split]]))
            levels(grid[[split]]) <- paste0(rep(titles[5], num_levels), ': ',
                levels(grid[[split]]))
        }
    }
    
    if (is.null(lines)) {
        lines <- 'g'  # dummy group so ggplot still draws lines
        draw.legend <- FALSE
    }
    
    # build and return graph
    graph <- .build_plot(grid, y, x, lines, split, errors, ymin, ymax,
        titles, bargraph, draw.legend, dodge, exp)
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
#' @param titles: A character vector with strings for the various plot titles.
#'   In order: Graph title, 'y' title, 'x' title, 'lines' title', 'split' title.
#'   If any position is NULL, the names of the variables will be used.
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
.build_plot <- function(grid, y, x, lines=NULL, split=NULL, errors=TRUE,
    ymin=NULL, ymax=NULL, titles=NULL, bargraph=FALSE, draw.legend=TRUE,
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
            ggplot2::aes(ymax=error_upper, ymin=error_lower),
            width=0.1,
            position=pd)
    }
    
    # split graph by 'split' variable
    if (!is.null(split)) {
        graph <- graph + ggplot2::facet_grid(paste0('. ~ ', split))
    }
    
    # add titles to graph
    if (!is.null(titles)) {
        if (!is.null(titles[1]) && !is.na(titles[1])) {
            graph <- graph + ggplot2::ggtitle(titles[1])
        }
        if (!is.null(titles[2]) && !is.na(titles[2])) {
            graph <- graph + ggplot2::ylab(titles[2])
        }
        if (!is.null(titles[3]) && !is.na(titles[3])) {
            graph <- graph + ggplot2::xlab(titles[3])
        }
        if (!is.null(titles[4]) && !is.na(titles[4])) {
            graph <- graph + ggplot2::labs(colour=titles[4])
        }
    }
    return(graph)
}




