#' Graph fitted model interactions.
#' 
#' \code{graph_model} provides an easy way to graph interactions in fitted
#' models. Selected variables will be graphed at +/- 1 SD (if continuous) or at
#' each level of the factor (if categorical).
#' 
#' @param model A fitted linear model of type 'lm'.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given model.
#' @return A ggplot2 graph of the plotted variables in the model.
#' @seealso \code{\link{graph_model.lm}}
#' @examples TODO: Need to complete.
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
#' @examples TODO: Need to complete.
#' @export
graph_model.lm <- function(model, y, x, lines=NULL, split=NULL, errorbars='CI',
    ymin=NULL, ymax=NULL, titles=NULL, bargraph=FALSE, draw.legend=TRUE,
    dodge=0, exp=FALSE) {
    
    call <- as.list(match.call())[-1]
    
    # convert variable names to strings
    call$y <- deparse(substitute(y))
    call$x <- deparse(substitute(x))
    if (!is.null(lines)) {
        call$lines <- deparse(substitute(lines))
    }
    if (!is.null(split)) {
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
#' @return A ggplot2 graph of the plotted variables in the model.
#' @examples TODO: Need to complete.
#' @export
graph_model_q.lm <- function(model, y, x, lines=NULL, split=NULL,
    errorbars='CI', ymin=NULL, ymax=NULL, titles=NULL, bargraph=FALSE,
    draw.legend=TRUE, dodge=0, exp=FALSE) {
    
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
    
    # set up the grid of points to graph the model at, to use to predict
    # cell means
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
        names(grid)[names(grid) == 'x'] <- x
        names(grid)[names(grid) == 'lines'] <- lines
    } else {
        grid <- with(data, expand.grid(
            x=factors[[1]],
            lines=factors[[2]],
            split=factors[[3]]
        ))
        names(grid)[names(grid) == 'x'] <- x
        names(grid)[names(grid) == 'lines'] <- lines
        names(grid)[names(grid) == 'split'] <- split
    }
    
    # add in other variables that are in the model, but are not selected to be
    # graphed; these variables get plotted at their means
    variables <- all.vars(formula(model))
    factorName <- NULL
    for (i in 1:length(variables)) {
        if (i > 1) {  # skip over 1, which is the DV
            if (!(variables[[i]] %in% colnames(grid))) {
                if (is.factor(data[[variables[[i]]]])) {
                    # if factor, must include all levels in model for
                    # predict() to work properly
                    tempList <- lapply(as.list(grid), unique)
                    # get unique values and put into list format
                    tempList[[variables[[i]]]] <- levels(data[[variables[[i]]]])
                    grid <- expand.grid(tempList)
                    factorName <- variables[[i]]
                } else {
                    grid[[variables[[i]]]] <- mean(data[[variables[[i]]]],
                        na.rm=TRUE)  # if continuous, include in model at mean
                }
            }
        }
    }
    
    # predict cell means
    predicted <- predict(model, newdata=grid, se=TRUE)
    if (exp == TRUE) {
        grid[[y]] <- exp(predicted$fit)
    } else {
        grid[[y]] <- predicted$fit
    }
    
    # add error bars, if desired
    errors <- FALSE
    if (errorbars == 'CI' || errorbars == 'ci') {
        grid$error.upper <- predicted$fit + 1.96 * predicted$se.fit
        grid$error.lower <- predicted$fit - 1.96 * predicted$se.fit
        errors <- TRUE
    } else if (errorbars == 'SE' || errorbars == 'se') {
        grid$error.upper <- predicted$fit + predicted$se.fit
        grid$error.lower <- predicted$fit - predicted$se.fit
        errors <- TRUE
    }
    if (exp == TRUE && errors == TRUE) {
        grid$error.upper <- exp(grid$error.upper)
        grid$error.lower <- exp(grid$error.lower)
    }
    
    # workaround to include factor covariates -- average across levels;
    # only works for one factor covariate
    if (!is.null(factorName)) {
        grid2 <- subset(grid, FALSE)
        lev <- levels(data[[factorName]])
        location <- which(names(grid) == factorName)
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
            numLevels <- length(levels(grid[[split]]))
            levels(grid[[split]]) <- paste0(rep(titles[5], numLevels), ': ',
                levels(grid[[split]]))
        }
    }
    
    if (is.null(lines)) {
        lines <- 'g'  # dummy group so ggplot still draws lines
        draw.legend <- FALSE
    }
    
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
        pd <- ggplot2::position_dodge(dodge + .9)
        # default dodge value to separate bars
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
            ggplot2::aes(ymax=error.upper, ymin=error.lower),
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




