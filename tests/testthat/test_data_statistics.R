context("Data statistics")
library(dplyr)

test_that("Rows is number of rows", {
    expect_equal(emc_data_statistics(iris)$rows, 150)
})

test_that("Cols is number of column", {
    expect_equal(emc_data_statistics(iris)$cols, 5)
})

test_that("Empty data frame returns zeros", {
    stats <- emc_data_statistics(tibble::tibble())
    expect_equal(stats$rows, 0)
    expect_equal(stats$cols, 0)
})
