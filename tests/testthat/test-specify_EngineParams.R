test_that("QRPEM parameters are properly specified", {
  EstArgs <-
    specify_EngineParams(
      sort = TRUE,
      ODE = "DVERK",
      rtolODE = 1e-5,
      atolODE = 1e-5,
      maxStepsODE = 6000,
      numIterations = 100,
      method = "QRPEM",
      numIterMAPNP = 3,
      stdErr = "Fisher-Score",
      isCentralDiffStdErr = FALSE,
      iSample = 350,
      impDist = "Mixture-2",
      scramble = "Tezuka-Faur"
    )

  testthat::expect_snapshot_value(EstArgs)
})

test_that("Default EstArgs are properly specified", {
  EstArgs <-
    specify_EngineParams(
    )

  testthat::expect_snapshot_value(EstArgs)
})
