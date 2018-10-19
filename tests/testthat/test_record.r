context("Record and Replay")
library(dplyr)

test_that("Record value", {

    recording <- emc_record(A = 1)
    expect_equal(nrow(recording), 1)
    expect_equal(recording$A, list(1))
    expect_equal(colnames(recording), c("A", "A_log", "A_error", "A_errors", "A_messages", "A_warnings", "A_duration"))

})
