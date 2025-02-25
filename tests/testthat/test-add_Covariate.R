test_that("add_Covariate function tests", {

  # Test case: Basic test with default arguments
  PMLParametersSets <- create_ModelPK()
  PMLParametersSets <- add_Covariate(PMLParametersSets, Name = "WT")
  expect_equal(unique(.gather_ClassProperties(PMLParametersSets, "Covariate", "Name", c())), "WT")

  # Test case: Substituting a covariate
  expect_message(add_Covariate(PMLParametersSets, Name = "WT", StParmNames = "V", State = "Searched"))
  #
  # Test case: Adding a covariate with specific parameters
  PMLParametersSets <-
    add_Covariate(PMLParametersSets,
                  Name = "Age",
                  Type = "Categorical",
                  StParmNames = "Cl",
                  State = "Present",
                  Direction = "Backward",
                  Center = "None",
                  Categories = c(1, 2, 3),
                  PMLStructures = "PK1IVC")

  expect_snapshot_value(PMLParametersSets, style = "json2")
  #
  # Test case: Invalid StParmNames
  expect_error(add_Covariate(PMLParametersSets, "Cov", StParmNames = "InvalidStParm"))

  # Test case: Invalid PMLStructures
  expect_error(add_Covariate(PMLParametersSets, "WT", PMLStructures = "InvalidStructure"))


  models <- create_ModelPK(CompartmentsNumber = c(1, 2, 3))

  models <-
    modify_Theta(models, Name = "tvCl3", InitialEstimates = 5)

  models <-
    add_Covariate(
      models,
      Name = "Occasion",
      Type = "Occasion",
      Categories = c(1, 2, 3),
      State = "Present",
      StParmNames = "V3"
    )

  models <-
    modify_Theta(models, Name = "tvV", InitialEstimates = 5)


  models <-
    add_Covariate(
      models,
      Name = "Sex",
      Type = "Categorical",
      Categories = c(0, 1),
      State = "Searched",
      StParmNames = "V2"
    )

  models <-
    modify_Theta(models, Name = "tvCl", InitialEstimates = 5)

  models <-
    add_Covariate(models,
                  Name = "BW",
                  Center = 70,
                  StParmNames = "V")

  expect_snapshot_output(models)
})

test_that("remove_Covariate function tests", {
  PMLParametersSets <- create_ModelPK(CompartmentsNumber = c(1, 2, 3))
  PMLParametersSets <- add_Covariate(PMLParametersSets, Name = "WT")
  expect_snapshot_value(print(remove_Covariate(PMLParametersSets, "WT", StParmNames = "V")),
                        style = "json2")
})
