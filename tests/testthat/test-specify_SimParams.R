test_that("Simulation parameters are properly specified", {
  SimArgs <-
    specify_SimParams(
      sort = TRUE,
      ODE = "DVERK",
      rtolODE = 1e-5,
      atolODE = 1e-5,
      maxStepsODE = 500,
      numReplicates = 100L,
      seed = 14L
    )

  testthat::expect_snapshot_value(SimArgs)
})

test_that("Default SimArgs are properly specified", {
  SimArgs <-
    specify_SimParams(
    )

  testthat::expect_snapshot_value(SimArgs)
})
