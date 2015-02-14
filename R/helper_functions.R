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
.set_factors <- function(model, int_vars, user_levels=NULL, sstest=TRUE) {
    factors <- list()
    for (term in int_vars) {
        term_data <- model$model[[term]]
        if (!is.null(user_levels) && term %in% names(user_levels)) {
            # if user specified levels, use those
            factors[[term]] <- user_levels[[term]]
        } else {
            if (attr(terms(model), 'dataClasses')[term] == 'factor') {
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
.offset_point <- function(var, offset_sd, digits=6) {
    point <- mean(var, na.rm=TRUE) + offset_sd * sd(var, na.rm=TRUE)
    return(round(point, digits))
}


#' Remove and insert columns in data frame
#' 
#' Helper function allows columns to be removed and added at a particular splice
#' point. This function works similar to the Javascript function splice().
#' 
#' @param df The data frame to modify
#' @param splice_point The column at which to make changes. Removed columns will
#'   start with this one, and new columns will be added starting at this column
#'   index.
#' @param num_remove The number of columns to remove.
#' @param new_cols A data frame of new columns to be added.
#' @return A new data frame with the modifications made.
.df_splice <- function(df, splice_point, num_remove=0, new_cols=NULL) {
    old_colnames <- colnames(df)
    
    # remove columns
    if (splice_point > 0 && splice_point <= ncol(df) &&
            num_remove > 0 && (splice_point + num_remove - 1) <= ncol(df)) {
        
        new_df <- as.data.frame(df[,
            -c(splice_point:(splice_point+num_remove-1))])
        
        colnames(new_df) <- old_colnames[
            -c(splice_point:(splice_point+num_remove-1))]
    } else {
        new_df <- df
    }
    
    # add new columns
    if (!is.null(new_cols)) {
        new_cols <- as.data.frame(new_cols)
        
        if (splice_point == 1) {
            new_df2 <- cbind(new_cols, new_df)
        } else {
            new_df2 <- cbind(
                new_df[, 1:(new_splice-1)],
                new_cols,
                new_df[, -c(1:(new_splice-1))])
        }
    } else {
        new_df2 <- new_df
    }
    return(new_df2)
}


#' Remove and insert rows in data frame
#' 
#' Helper function allows rows to be removed and added at a particular splice
#' point. This function works similar to the Javascript function splice().
#' 
#' @param df The data frame to modify
#' @param splice_point The column at which to make changes. Removed rows will
#'   start with this one, and new rows will be added starting at this row index.
#' @param num_remove The number of rows to remove.
#' @param new_rows A data frame of new rows to be added.
#' @param reset_rownames Whether or not to renumber rownames when finished.
#' @return A new data frame with the modifications made.
.df_row_splice <- function(df, splice_point, num_remove=0, new_rows=NULL,
    reset_rownames=FALSE) {
    old_rownames <- rownames(df)
    
    # remove rows
    if (splice_point > 0 && splice_point <= nrow(df) &&
            num_remove > 0 && (splice_point + num_remove - 1) <= nrow(df)) {
        
        new_df <- as.data.frame(
            df[-c(splice_point:(splice_point+num_remove-1)), ])
    } else {
        new_df <- df
    }
    
    # add new rows
    if (!is.null(new_rows)) {
        new_rows <- as.data.frame(new_rows)
        new_rownames <- rownames(new_df)
        
        if (splice_point == 1) {
            new_df2 <- rbind(new_rows, new_df)
        } else {
            new_df2 <- rbind(
                new_df[1:(splice_point-1), ],
                new_rows,
                new_df[-c(1:(splice_point-1)), ])
        }
        
        if (reset_rownames) {
            rownames(new_df2) <- 1:nrow(new_df2)
        } else {
            # check for rowname conflicts
            if (any(rownames(new_rows) %in% rownames(new_df))) {
                warning('Rownames in new rows conflict with original rownames. Rownames will be reset.')
                rownames(new_df2) <- 1:nrow(new_df2)
            }
        }
    } else {
        new_df2 <- new_df
        if (reset_rownames) {
            rownames(new_df2) <- 1:nrow(new_df2)
        }
    }
    return(new_df2)
}




