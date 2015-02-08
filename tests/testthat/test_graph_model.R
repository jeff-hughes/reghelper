context('graph_model function')


test_that('lm with 2 continuous int. works', {
    set.seed(123)
    x1 <- rnorm(100)
    
    set.seed(234)
    x2 <- rnorm(100)
    
    set.seed(345)
    y <- x1 * x2 + rnorm(100)
    
    model <- lm(y ~ x1 * x2)
    graph <- graph_model(model, y=y, x=x1, lines=x2)
    
    expect_is(graph, 'ggplot')
    expect_equal(length(graph$layers), 3)  # geom_point, geom_line, geom_errorbar
    
    expect_equal(as.character(graph$mapping$x), 'x1')
    expect_equal(as.character(graph$mapping$y), 'y')
    expect_equal(as.character(graph$mapping$colour), 'x2')
    
    expect_equal(levels(graph$data$x1), c('-1 SD', '+1 SD'))
    expect_equal(levels(graph$data$x2), c('-1 SD', '+1 SD'))
})


test_that('lm with continuous x 2-level categorical int. works', {
    set.seed(123)
    x1 <- rnorm(100)
    
    x2 <- c(rep(0, 50), rep(1, 50))
    
    set.seed(345)
    y <- x1 * x2 + rnorm(100)
    
    x2 <- factor(x2)
    
    model <- lm(y ~ x1 * x2)
    graph <- graph_model(model, y=y, x=x1, lines=x2)
    
    expect_is(graph, 'ggplot')
    expect_equal(length(graph$layers), 3)  # geom_point, geom_line, geom_errorbar
    
    expect_equal(as.character(graph$mapping$x), 'x1')
    expect_equal(as.character(graph$mapping$y), 'y')
    expect_equal(as.character(graph$mapping$colour), 'x2')
    
    expect_equal(levels(graph$data$x1), c('-1 SD', '+1 SD'))
    expect_equal(levels(graph$data$x2), c('0', '1'))
})


test_that('lm with continuous x 3-level categorical int. works', {
    set.seed(123)
    x1 <- rnorm(150)
    
    x2 <- c(rep(0, 50), rep(1, 50), rep(2, 50))
    
    set.seed(345)
    y <- x1 * x2 + rnorm(150)
    
    x2 <- factor(x2)
    
    model <- lm(y ~ x1 * x2)
    graph <- graph_model(model, y=y, x=x1, lines=x2)
    
    expect_is(graph, 'ggplot')
    expect_equal(length(graph$layers), 3)  # geom_point, geom_line, geom_errorbar
    
    expect_equal(as.character(graph$mapping$x), 'x1')
    expect_equal(as.character(graph$mapping$y), 'y')
    expect_equal(as.character(graph$mapping$colour), 'x2')
    
    expect_equal(levels(graph$data$x1), c('-1 SD', '+1 SD'))
    expect_equal(levels(graph$data$x2), c('0', '1', '2'))
})


test_that('lm with 3 continuous int. works', {
    set.seed(123)
    x1 <- rnorm(100)
    
    set.seed(234)
    x2 <- rnorm(100)
    
    set.seed(345)
    x3 <- rnorm(100)
    
    set.seed(456)
    y <- x1 * x2 * x3 + rnorm(100)
    
    model <- lm(y ~ x1 * x2 * x3)
    graph <- graph_model(model, y=y, x=x1, lines=x2, split=x3)
    
    expect_is(graph, 'ggplot')
    expect_equal(length(graph$layers), 3)  # geom_point, geom_line, geom_errorbar
    
    expect_equal(as.character(graph$mapping$x), 'x1')
    expect_equal(as.character(graph$mapping$y), 'y')
    expect_equal(as.character(graph$mapping$colour), 'x2')
    expect_equal(names(graph$facet$cols), 'x3')
    
    expect_equal(levels(graph$data$x1), c('-1 SD', '+1 SD'))
    expect_equal(levels(graph$data$x2), c('-1 SD', '+1 SD'))
    expect_equal(levels(graph$data$x3), c('-1 SD', '+1 SD'))
})



