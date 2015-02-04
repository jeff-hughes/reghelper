context('block_model function')

# takes model and returns coefficient for a given variable/row number, rounded
# to 'digits' decimal places
get_coef <- function(model, row, digits=3) {
    return(round(coef(model)[row, 1], digits))
}


test_that('2 block lm, no interaction, works', {
    set.seed(123)
    x1 <- rnorm(100)
    
    set.seed(234)
    x2 <- rnorm(100)
    
    set.seed(345)
    y <- x1 + x2 + rnorm(100)
    
    model1 <- summary(lm(y ~ x1))
    model2 <- summary(lm(y ~ x1 + x2))
    
    b_model <- block_lm('y', blocks=list('x1', 'x2'))
    
    expect_equal(length(b_model$formulas), 2)
    expect_equal(length(b_model$models), 2)
    
    b_summ <- summary(b_model)
    
    expect_equal(nrow(b_summ$residuals), 2)
    
    # check coefficients
    expect_equal(round(coef(b_summ, 1)['x1', 1], 3), get_coef(model1, 'x1'))
    expect_equal(round(coef(b_summ, 2)['x1', 1], 3), get_coef(model2, 'x1'))
    expect_equal(round(coef(b_summ, 2)['x2', 1], 3), get_coef(model2, 'x2'))
    
    # check r-squared
    expect_equal(round(b_summ$overall[1, 1], 3),
        round(model1$r.squared, 3))
    expect_equal(round(b_summ$overall[2, 1], 3),
        round(model2$r.squared, 3))
})


test_that('2 block lm, with interaction, works', {
    set.seed(123)
    x1 <- rnorm(100)
    
    set.seed(234)
    x2 <- rnorm(100)
    
    set.seed(345)
    y <- x1 * x2 + rnorm(100)
    
    model1 <- summary(lm(y ~ x1 + x2))
    model2 <- summary(lm(y ~ x1 * x2))
    
    b_model <- block_lm('y', blocks=list(c('x1', 'x2'), 'x1 * x2'))
    
    expect_equal(length(b_model$formulas), 2)
    expect_equal(length(b_model$models), 2)
    
    b_summ <- summary(b_model)
    
    expect_equal(nrow(b_summ$residuals), 2)
    
    # check coefficients
    #expect_equal(round(coef(b_summ, 1)['x1', 1], 3), get_coef(model1, 'x1'))
    expect_equal(round(coef(b_summ, 1)['x1', 1], 3), round(coef(model1)['x1', 1], 3))
    expect_equal(round(coef(b_summ, 1)['x2', 1], 3), get_coef(model1, 'x2'))
    expect_equal(round(coef(b_summ, 2)['x1', 1], 3), get_coef(model2, 'x1'))
    expect_equal(round(coef(b_summ, 2)['x2', 1], 3), get_coef(model2, 'x2'))
    expect_equal(round(coef(b_summ, 2)['x1:x2', 1], 3),
        get_coef(model2, 'x1:x2'))
    
    # check r-squared
    expect_equal(round(b_summ$overall[1, 1], 3),
        round(model1$r.squared, 3))
    expect_equal(round(b_summ$overall[2, 1], 3),
        round(model2$r.squared, 3))
})


