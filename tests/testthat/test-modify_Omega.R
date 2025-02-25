test_that("Modify Omega in PMLParametersSets", {
  # Create a sample PMLParametersSets (replace with your data if necessary)
  PMLParametersSets <-
    get_PMLParametersSets(Absorption = "First-Order",
                          Saturation = c(TRUE, FALSE))

  expect_snapshot(
    modify_Omega(PMLParametersSets,
                 Name = "nV",
                 InitialOmega = 0.5,
                 Frozen = TRUE,
                 State = "Present",
                 PMLStructures = "PK1FOCS")
  )




  })
