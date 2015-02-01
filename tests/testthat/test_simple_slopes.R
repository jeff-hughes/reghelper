context('simple_slopes function')

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
    expect_equal(slopes[1, 't value'], coef(model_x1_m1)['x2', 't value'])
    
    model_x1_0 <- summary(lm(y ~ I(x1 - mean(x1)) * x2))
    expect_equal(slopes[2, 't value'], coef(model_x1_0)['x2', 't value'])
    
    model_x1_p1 <- summary(lm(y ~ I((x1 - mean(x1)) - sd(x1)) * x2))
    expect_equal(slopes[3, 't value'], coef(model_x1_p1)['x2', 't value'])
    
    model_x2_m1 <- summary(lm(y ~ x1 * I((x2 - mean(x2)) + sd(x2))))
    expect_equal(slopes[4, 't value'], coef(model_x2_m1)['x1', 't value'])
    
    model_x2_0 <- summary(lm(y ~ x1 * I(x2 - mean(x2))))
    expect_equal(slopes[5, 't value'], coef(model_x2_0)['x1', 't value'])
    
    model_x2_p1 <- summary(lm(y ~ x1 * I((x2 - mean(x2)) - sd(x2))))
    expect_equal(slopes[6, 't value'], coef(model_x2_p1)['x1', 't value'])
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
    expect_equal(slopes[1, 't value'], coef(model_x1_m1)['x21', 't value'])
    
    model_x1_0 <- summary(lm(y ~ I(x1 - mean(x1)) * x2))
    expect_equal(slopes[2, 't value'], coef(model_x1_0)['x21', 't value'])
    
    model_x1_p1 <- summary(lm(y ~ I((x1 - mean(x1)) - sd(x1)) * x2))
    expect_equal(slopes[3, 't value'], coef(model_x1_p1)['x21', 't value'])
    
    model_x2_0 <- summary(lm(y ~ x1 * x2))  # TODO: function fails when one of cat. levels is named '0'
    expect_equal(slopes[4, 't value'], coef(model_x2_0)['x1', 't value'])
    
    contrasts(x2) <- c(1, 0)
    model_x2_1 <- summary(lm(y ~ x1 * x2))
    expect_equal(slopes[5, 't value'], coef(model_x2_1)['x1', 't value'])
})


