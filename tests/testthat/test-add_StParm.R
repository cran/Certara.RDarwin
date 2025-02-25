test_that("add_StParm remove_StParm work correctly", {
  # Create a test PMLParametersSets object
  TestPml <- get_PMLParametersSets(CompartmentsNumber = c(1, 2))

  # substitute a structural parameter using add_StParm
  expect_warning(
    modify_StParm(
      TestPml,
      StParmName = "Vd",
      Type = "LogNormal",
      State = "Present",
      PMLStructures = "PK2IVC"
    )
  )

  expect_error(
    TestPml1 <-
      add_StParm(
        TestPml,
        StParmName = "duration",
        Type = "LogNormal",
        State = "Present",
        DosepointArgName = NULL
      )
  )

  TestPml1 <- modify_StParm(TestPml,
                            StParmName = "V",
                            Type = "Normal")

  expect_snapshot(TestPml1)

  TestPml <- add_StParm(
    TestPml,
    StParmName = "duration",
    Type = "LogNormal2",
    State = "Searched",
    DosepointArgName = "duration",
    PMLStructures = "PK1IVC"
  )

  # Test that the structural parameter was added correctly
  expect_true(TestPml$PK1IVC$MainDosepoint$A1$duration$StParmName == "duration")

  TestPml <- remove_StParm(TestPml,
                           StParmName = "duration")

  expect_true(length(TestPml$PK1IVC$MainDosepoint$A1$duration) == 0)
})
