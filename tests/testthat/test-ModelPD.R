test_that("create_ModelPD works with different options", {
  # Default Emax model
  emax_default <- create_ModelPD(Type = "Emax")
  expect_equal(names(emax_default), "Emax")
  expect_equal(emax_default$Emax$StParms,
               list(
                 Emax = StParm(StParmName = "Emax",
                               PMLStructure = "Emax"),
                 EC50 = StParm(StParmName = "EC50",
                               PMLStructure = "Emax")
               ))

  expect_equal(emax_default$Emax$Observations, list(
    EObs = Observation(
      ObservationName = "EObs",
      PMLStructure = "Emax",
      Covariates = Covariate(Name = "C")
    )
  ))

  # Emax model with baseline
  emax_baseline <- create_ModelPD(Type = "Emax", Baseline = TRUE)
  expect_equal(names(emax_baseline), "EmaxE0")
  expect_equal(emax_baseline$EmaxE0$StParms, list(
    E0 = StParm(
      StParmName = "E0",
      PMLStructure = "EmaxE0"
    ),
    Emax = StParm(
      StParmName = "Emax",
      PMLStructure = "EmaxE0"
    ),
    EC50 = StParm(
      StParmName = "EC50",
      PMLStructure = "EmaxE0"
    )
  ))

  # Emax model with fractional
  expect_warning(create_ModelPD(Type = "Emax", Baseline = FALSE, Fractional = TRUE))

  # Emax model with inhibitory
  emax_inhibitory <- create_ModelPD(Type = "Emax", Inhibitory = TRUE)
  expect_equal(names(emax_inhibitory), "Imax")
  expect_equal(emax_inhibitory$Imax$StParms, list(
    E0 = StParm(
      StParmName = "E0",
      PMLStructure = "Imax"
    ),
    IC50 = StParm(
      StParmName = "IC50",
      PMLStructure = "Imax"
    )
  ))

  # Emax model with sigmoid
  emax_sigmoid <- create_ModelPD(Type = "Emax", Sigmoid = TRUE)
  expect_equal(names(emax_sigmoid), "EmaxGam")
  expect_equal(emax_sigmoid$EmaxGam$StParms, list(
    Emax = StParm(
      StParmName = "Emax",
      PMLStructure = "EmaxGam"
    ),
    Gam = StParm(
      StParmName = "Gam",
      PMLStructure = "EmaxGam"
    ),
    EC50 = StParm(
      StParmName = "EC50",
      PMLStructure = "EmaxGam"
    )
  ))

  # Emax model with all options
  emax_all <- create_ModelPD(
    Type = "Emax",
    Baseline = TRUE,
    Fractional = TRUE,
    Inhibitory = TRUE,
    Sigmoid = TRUE
  )
  expect_equal(names(emax_all), "ImaxE01+Gam")
  expect_equal(emax_all$`ImaxE01+Gam`$StParms, list(
    E0 = StParm(
      StParmName = "E0",
      PMLStructure = "ImaxE01+Gam"
    ),
    Imax = StParm(
      StParmName = "Imax",
      PMLStructure = "ImaxE01+Gam"
    ),
    Gam = StParm(
      StParmName = "Gam",
      PMLStructure = "ImaxE01+Gam"
    ),
    IC50 = StParm(
      StParmName = "IC50",
      PMLStructure = "ImaxE01+Gam"
    )
  ))
})

test_that("create_ModelPD works with ByVector", {
  # ByVector FALSE with all possible combinations
  expect_warning(emax_by_vector <- create_ModelPD(
    Type = "Emax",
    Baseline = c(FALSE, TRUE),
    Fractional = c(FALSE, TRUE),
    Inhibitory = c(FALSE, TRUE),
    Sigmoid = c(FALSE, TRUE),
    ByVector = FALSE
  ))

  expect_equal(
    names(emax_by_vector),
    c(
      "Emax",
      "EmaxGam",
      "Imax",
      "ImaxGam",
      "EmaxE0",
      "EmaxE0Gam",
      "ImaxE0",
      "ImaxE0Gam",
      "EmaxE01+",
      "EmaxE01+Gam",
      "ImaxE01+",
      "ImaxE01+Gam"
    )
  )

  # ByVector TRUE with single value for each option
  emax_by_vector_single <- create_ModelPD(
    Type = "Emax",
    Baseline = FALSE,
    Fractional = FALSE,
    Inhibitory = FALSE,
    Sigmoid = FALSE,
    ByVector = TRUE
  )
  expect_equal(names(emax_by_vector_single), "Emax")

  # ByVector TRUE with one option TRUE and the others FALSE
  emax_by_vector_one_true <- create_ModelPD(
    Type = "Emax",
    Baseline = FALSE,
    Fractional = FALSE,
    Inhibitory = FALSE,
    Sigmoid = TRUE,
    ByVector = TRUE
  )
  expect_equal(names(emax_by_vector_one_true), "EmaxGam")

  # ByVector TRUE with all options FALSE
  emax_by_vector_all_false <- create_ModelPD(
    Type = "Emax",
    Baseline = FALSE,
    Fractional = FALSE,
    Inhibitory = FALSE,
    Sigmoid = FALSE,
    ByVector = TRUE
  )
  expect_equal(names(emax_by_vector_all_false), "Emax")
})

test_that("create_ModelPD works with additional arguments", {
  # Add a covariate to all models
  emax_with_covariate <- create_ModelPD(
    Type = "Emax",
    WT = Covariate(Name = "WT",
                PMLStructure = "Emax")
    )

  expect_equal(
    emax_with_covariate$Emax$StParms$Emax$Covariates[[1]]$Name,
    "WT"
  )

  # Modify an observation in a specific model
  emax_modified_observation_specific <- create_ModelPD(
    Type = "Emax",
    EObs = Observation(ObservationName = "EObs",
                  PMLStructure = "Emax",
                  SigmasChosen = Sigmas(Additive = 1)
    )
  )
  expect_equal(
    emax_modified_observation_specific$Emax$Observations$EObs$SigmasChosen$Additive,
    1
  )
})

