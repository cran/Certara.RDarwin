test_that("Dosepoint creates a valid Dosepoint object", {
  ObservationInstance <- Observation(ObservationName = "A0Obs",
                                   SigmasChosen = list(
                                     Additive = 2,
                                     LogAdditive = 0,
                                     Proportional = 0.1,
                                     AdditiveMultiplicative = c(PropPart = 0, AddPart = 0),
                                     MixRatio = c(PropPart = 0, AddPart = 0),
                                     Power = c(Stdev = 0, Power = 0)
                                   ),
                                   Frozen = FALSE,
                                   ResetObs = TRUE,
                                   PMLStructure = "1Cpt")

  expect_output(cat(output(ObservationInstance)))

  ObservationInstance1 <- Observation(ObservationName = "CObs",
                                     SigmasChosen = list(
                                       Additive = 0,
                                       LogAdditive = 0,
                                       Proportional = 0,
                                       AdditiveMultiplicative = c(PropPart = 0, AddPart = 0),
                                       MixRatio = c(PropPart = 1, AddPart = 10),
                                       Power = c(Stdev = 0, Power = 0)
                                     ),
                                     Frozen = TRUE,
                                     ResetObs = FALSE,
                                     PMLStructure = "1Cpt")

  expect_output(cat(output(ObservationInstance1)))

})
