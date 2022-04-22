#' Find points to test variables.
#' 
#' Helper function calculates points at which to test slopes of a variable. For
#' categorical variables, this includes all its levels. For continuous
#' variables, this is -1 SD, the mean, and +1 SD.
#' 
#' @param model The linear model being tested.
#' @param int_vars The variables involved in the interaction being tested.
#' @param user_levels Any user-specified levels to be used instead of the
#'   defaults.
#' @param sstest Logical. Whether or not to insert 'sstest' as a factor level.
#' @return A list with all the factor points for each variable in the
#'   interaction.
#' @noRd
.set_factors <- function(model, int_vars, user_levels=NULL, sstest=TRUE) {
    factors <- list()
    for (term in int_vars) {
        term_data <- model[[term]]
        if (!is.null(user_levels) && term %in% names(user_levels)) {
            # if user specified levels, use those
            factors[[term]] <- user_levels[[term]]
        } else {
            if (class(term_data) == 'factor') {
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


#' Calculate points for testing simple slopes
#' 
#' Helper function calculates point at which to set a variable, based on its
#' mean and standard deviation.
#' 
#' @param var A vector of the variable to test.
#' @param offset_sd The value, in standard deviations, at which you wish to
#'   offset the variable.
#' @param digits Number of decimal places to round value.
#' @return The value of the variable at the offset point. For example, for a
#'   standard normal distribution, an offset point of -1 would return a value of
#'   -1.
#' @noRd
.offset_point <- function(var, offset_sd, digits=6) {
    point <- mean(var, na.rm=TRUE) + offset_sd * sd(var, na.rm=TRUE)
    return(round(point, digits))
}


#' Convert an expression-like function input into a string.
#' 
#' Helper function to assist with non-standard evaluation, converting expressions
#' and symbols into strings.
#' 
#' @param expr The expression to change.
#' @return A string version of the expression.
#' @noRd
.expr_to_str <- function(expr) {
    # we need the paste because `deparse` will insert line breaks for
    # long expressions, which is definitely not useful to us
    return(paste(deparse(expr, width.cutoff=500), collapse=""))
}



