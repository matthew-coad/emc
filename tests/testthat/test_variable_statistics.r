context("Variable exploration")


test_that("Iris variable distribution returns expected result", {
    expect_known_value(emc_variable_distribution(iris), "iris_variable_distribution.rds", update=FALSE)
})

test_that("mtcars variable degeneracy returns expected result", {
    data(BreastCancer, package="mlbench")
    expect_known_value(emc_variable_degeneracy(BreastCancer), "Breast_Cancer_degeneracy.rds", update=FALSE)
})

test_that("Iris levels expected result", {
    expect_known_value(emc_level_statistics(iris), "Iris_Level_Statistics.rds", update=FALSE)
})


