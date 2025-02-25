# Test for modify_Observation
test_that("modify_Observation correctly modifies an Observation", {
  # Create a sample PMLParametersSets
  PMLParametersSets <- create_ModelPK()

  # Modify an Observation
  modified_sets <- modify_Observation(
    PMLParametersSets,
    ObservationName = "CObs",
    SigmasChosen =
      Sigmas(
        Proportional = 0,
        AdditiveMultiplicative = list(PropPart = 0.1, AddPart = 10)
      )
  )

  # Check if the modification was successful
  expect_identical(
    modified_sets$PK1IVC$Observations$CObs,
    Observation(
      ObservationName = "CObs",
      SigmasChosen =
        Sigmas(
          Proportional = 0,
          AdditiveMultiplicative = list(PropPart = 0.1, AddPart = 10)
        ),
      PMLStructure = "PK1IVC"
    )
  )

  expect_warning(modify_Observation(
    PMLParametersSets,
    ObservationName = "AObs",
    SigmasChosen =
      Sigmas(
        Proportional = 1
      )
  ))

  PMLParametersSets <- create_ModelPK(EliminationCpt = c(FALSE, TRUE))

  expect_warning(modify_Observation(
    PMLParametersSets,
    ObservationName = "A0Obs",
    SigmasChosen =
      Sigmas(
        Proportional = 1
      ),
    PMLStructures = "PK1IVC"
  ))
})

# Test for remove_Observation
test_that("remove_Observation correctly removes an Observation", {
  # Create a sample PMLParametersSets
  PMLParametersSets <- create_ModelPK()

  # Remove an Observation
  modified_sets <-
    remove_Observation(PMLParametersSets, ObservationName = "CObs")

  # Check if the Observation is removed
  expect_true(!"CObs" %in% names(modified_sets$PK1IVC$Observations))
})

# Test for list_Observations
test_that("list_Observations correctly lists Observations", {
  # Create a sample PMLParametersSets
  PMLParametersSets <- create_ModelPK(EliminationCpt = TRUE)

  # Check if the list contains the expected Observation names
  expect_equal(list_Observations(PMLParametersSets),
               c("CObs", "A0Obs"))
})
