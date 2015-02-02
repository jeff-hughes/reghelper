context('simple_slopes function')

# takes model and returns coefficient for a given variable/row number, rounded
# to 'digits' decimal places
get_coef <- function(model, row, digits=3) {
    return(round(coef(model)[row, 1], digits))
}


test_that('lm with 2 continuous int. works', {
    set.seed(123)
    x1 <- rnorm(100)
    
    set.seed(234)
    x2 <- rnorm(100)
    
    set.seed(345)
    y <- x1 * x2 + rnorm(100)
    
    model <- lm(y ~ x1 * x2)
    slopes <- simple_slopes(model)
    
    model_x1_m1 <- summary(lm(y ~ I((x1 - mean(x1)) + sd(x1)) * x2))
    expect_equal(round(slopes[1, 'Test Estimate'], 3),
                 get_coef(model_x1_m1, 'x2'))
    
    model_x1_0 <- summary(lm(y ~ I(x1 - mean(x1)) * x2))
    expect_equal(round(slopes[2, 'Test Estimate'], 3),
                 get_coef(model_x1_0, 'x2'))
    
    model_x1_p1 <- summary(lm(y ~ I((x1 - mean(x1)) - sd(x1)) * x2))
    expect_equal(round(slopes[3, 'Test Estimate'], 3),
                 get_coef(model_x1_p1, 'x2'))
    
    model_x2_m1 <- summary(lm(y ~ x1 * I((x2 - mean(x2)) + sd(x2))))
    expect_equal(round(slopes[4, 'Test Estimate'], 3),
                 get_coef(model_x2_m1, 'x1'))
    
    model_x2_0 <- summary(lm(y ~ x1 * I(x2 - mean(x2))))
    expect_equal(round(slopes[5, 'Test Estimate'], 3),
                 get_coef(model_x2_0, 'x1'))
    
    model_x2_p1 <- summary(lm(y ~ x1 * I((x2 - mean(x2)) - sd(x2))))
    expect_equal(round(slopes[6, 'Test Estimate'], 3),
                 get_coef(model_x2_p1, 'x1'))
})


test_that('lm with continuous x 2-level categorical int. works', {
    set.seed(123)
    x1 <- rnorm(100)

    x2 <- c(rep(0, 50), rep(1, 50))
    
    set.seed(345)
    y <- x1 * x2 + rnorm(100)
    
    x2 <- factor(x2)
    
    model <- lm(y ~ x1 * x2)
    slopes <- simple_slopes(model)
    
    model_x1_m1 <- summary(lm(y ~ I((x1 - mean(x1)) + sd(x1)) * x2))
    expect_equal(round(slopes[1, 'Test Estimate'], 3),
                 get_coef(model_x1_m1, 'x21'))
    
    model_x1_0 <- summary(lm(y ~ I(x1 - mean(x1)) * x2))
    expect_equal(round(slopes[2, 'Test Estimate'], 3),
                 get_coef(model_x1_0, 'x21'))
    
    model_x1_p1 <- summary(lm(y ~ I((x1 - mean(x1)) - sd(x1)) * x2))
    expect_equal(round(slopes[3, 'Test Estimate'], 3),
                 get_coef(model_x1_p1, 'x21'))
    
    model_x2_0 <- summary(lm(y ~ x1 * x2))
    expect_equal(round(slopes[4, 'Test Estimate'], 3),
                 get_coef(model_x2_0, 'x1'))
    
    contrasts(x2) <- c(1, 0)
    model_x2_1 <- summary(lm(y ~ x1 * x2))
    expect_equal(round(slopes[5, 'Test Estimate'], 3),
                 get_coef(model_x2_1, 'x1'))
})


test_that('lm with continuous x 3-level categorical int. works', {
    set.seed(123)
    x1 <- rnorm(150)
    
    x2 <- c(rep(0, 50), rep(1, 50), rep(2, 50))
    
    set.seed(345)
    y <- x1 * x2 + rnorm(150)
    
    x2 <- factor(x2)
    
    model <- lm(y ~ x1 * x2)
    slopes <- simple_slopes(model)
    
    model_x1_m1 <- summary(lm(y ~ I((x1 - mean(x1)) + sd(x1)) * x2))
    expect_equal(round(slopes[1, 'Test Estimate'], 3),
        get_coef(model_x1_m1, 'x21'))
    expect_equal(round(slopes[2, 'Test Estimate'], 3),
        get_coef(model_x1_m1, 'x22'))
    
    model_x1_0 <- summary(lm(y ~ I(x1 - mean(x1)) * x2))
    expect_equal(round(slopes[3, 'Test Estimate'], 3),
        get_coef(model_x1_0, 'x21'))
    expect_equal(round(slopes[4, 'Test Estimate'], 3),
        get_coef(model_x1_0, 'x22'))
    
    model_x1_p1 <- summary(lm(y ~ I((x1 - mean(x1)) - sd(x1)) * x2))
    expect_equal(round(slopes[5, 'Test Estimate'], 3),
        get_coef(model_x1_p1, 'x21'))
    expect_equal(round(slopes[6, 'Test Estimate'], 3),
        get_coef(model_x1_p1, 'x22'))
    
    model_x2_0 <- summary(lm(y ~ x1 * x2))
    expect_equal(round(slopes[7, 'Test Estimate'], 3),
        get_coef(model_x2_0, 'x1'))
    
    contrasts(x2) <- matrix(c(1, 0, 0, 0, 0, 1), nrow=3)
    model_x2_1 <- summary(lm(y ~ x1 * x2))
    expect_equal(round(slopes[8, 'Test Estimate'], 3),
        get_coef(model_x2_1, 'x1'))
    
    contrasts(x2) <- matrix(c(1, 0, 0, 0, 1, 0), nrow=3)
    model_x2_1 <- summary(lm(y ~ x1 * x2))
    expect_equal(round(slopes[9, 'Test Estimate'], 3),
        get_coef(model_x2_1, 'x1'))
})


