context("Binding")
library(dplyr)

test_that("Bind expression", {
    parameters <- tibble(A = c(1,2))
    results <- emc_bind(parameters, B = tibble(B = A * 2))
    expect_equal(nrow(results), 2)
    expect_equal(results$A, c(1,2))
    expect_equal(results$B, c(2,4))
})

