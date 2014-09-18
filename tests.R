library("testthat")

source("cachematrix.R")

context("Week 3 Programming assignment")

expect_tol <- function(...) { expect_equal(tolerance=0.005, ...) }

### Checking cachematrix functionality
test_that("Cachematrix works correctly",
{
    m1 <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow=2, ncol=2))
    expect_message(
        cacheSolve(m1),
        "Calculating inverse of a matrix")
    
    m2  <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow=2, ncol=2))
    res <- matrix(c(-2, 1, 1.5, -0.5), nrow=2, ncol=2)
    expect_equal(cacheSolve(m2), res)
    expect_equal(cacheSolve(m2), res)
})