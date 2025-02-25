test_that("Dosepoint creates a valid Dosepoint object", {
  DosepointInstance <- Dosepoint(
    DosepointName = "A1",
    State = "Present",
    tlag = StParm("Tlag"),
    bioavail = StParm("F"),
    duration = StParm("D"),
    PMLStructure = "1Cpt"
  )

  expect_snapshot(DosepointInstance)
})
