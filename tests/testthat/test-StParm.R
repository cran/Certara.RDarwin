# Create a mock Theta instance for testing purposes
ThetaInstance <- Theta(Name = "test", InitialEstimates = 0.1)

test_that("StParm returns an instance of the StParm class", {
  # Call the StParm function
  result <- StParm(StParmName = "V", ThetaStParm = ThetaInstance)

  # Verify that the result is of class StParm
  expect_true("StParm" %in% class(result))
})

test_that("StParm creates a default Theta instance when none is provided", {
  # Call the StParm function without a Theta instance
  result <- StParm(StParmName = "V")

  # Verify that the resulting StParm instance has a Theta instance
  expect_true("Theta" %in% class(result$ThetaStParm))
})

test_that("StParm creates a default Omega instance when none is provided", {
  # Call the StParm function without an Omega instance
  result <- StParm(StParmName = "V")

  # Verify that the resulting StParm instance has an Omega instance
  expect_true("Omega" %in% class(result$OmegaStParm))
})

test_that("StParm sets the StParmName for Theta and Omega instances", {
  # Call the StParm function with a custom StParmName
  result <- StParm(StParmName = "V", ThetaStParm = ThetaInstance)

  # Verify that the resulting Theta instance has the correct StParmName
  expect_equal(result$ThetaStParm$StParmName, "V")

  # Verify that the resulting Omega instance has the correct StParmName
  expect_equal(result$OmegaStParm$StParmName, "V")
})

test_that(
  "Compare output StParm with implicit and explicit thetas for present occasion covariate",
  {
    ClOmegasExplicit <- StParm(
      StParmName = "Cl",
      Covariates = Covariate(
        Name = "Period",
        Type = "Occasion",
        State = "Present",
        Categories = c(1, 2),
        Omegas = list(Omega(Name = "nClPeriodx1"),
                      Omega(Name = "nClPeriodx2"))
      ),
      PMLStructure = "1CFOE"
    )

    ClOmegasImplicit <- StParm(
      StParmName = "Cl",
      Covariates = Covariate(
        Name = "Period",
        Type = "Occasion",
        State = "Present",
        Categories = c(1, 2)
      ),
      PMLStructure = "1CFOE"
    )

    expect_equal(print(ClOmegasExplicit), print(ClOmegasImplicit))
  }
)

test_that(
  "Compare output StParm with implicit and explicit thetas for searched occasion covariate",
  {
    ClOmegasExplicit <- StParm(
      StParmName = "Cl",
      Covariates = Covariate(
        Name = "Period",
        Type = "Occasion",
        State = "Searched",
        Categories = c(1, 2),
        Omegas = list(Omega(Name = "nClPeriodx1"),
                      Omega(Name = "nClPeriodx2"))
      ),
      PMLStructure = "1CFOE"
    )

    expect_snapshot(ClOmegasExplicit)

    ClOmegasImplicit <- StParm(
      StParmName = "Cl",
      Covariates = Covariate(
        Name = "Period",
        Type = "Occasion",
        State = "Searched",
        Categories = c(1, 2)
      ),
      PMLStructure = "1CFOE"
    )

    expect_snapshot(ClOmegasImplicit)
  }
)

test_that(
  "Compare output StParm with implicit and explicit thetas for present categorical covariate ",
  {
    ClThetasExplicit <- StParm(
      StParmName = "Cl",
      Covariates = Covariate(
        Name = "Race",
        Type = "Categorical",
        State = "Present",
        Categories = c(1, 2, 3),
        Thetas = list(Theta(Name = "dCldRace2",
                            InitialEstimates = 0),
                      Theta(Name = "dCldRace3",
                            InitialEstimates = 0))
      ),
      PMLStructure = "1CFOE"
    )

    ClThetasImplicit <- StParm(
      StParmName = "Cl",
      Covariates = Covariate(
        Name = "Race",
        Type = "Categorical",
        State = "Present",
        Categories = c(1, 2, 3)
      ),
      PMLStructure = "1CFOE"
    )

    expect_equal(print(ClThetasExplicit), print(ClThetasImplicit))
  }
)

test_that(
  "Compare output StParm with implicit and explicit thetas for searched categorical covariate ",
  {
    ClThetasExplicit <- StParm(
      StParmName = "Cl",
      Covariates = Covariate(
        Name = "Race",
        Type = "Categorical",
        State = "Searched",
        Categories = c(1, 2, 3),
        Thetas = list(Theta(Name = "dCldRace2"),
                      Theta(Name = "dCldRace3"))
      ),
      PMLStructure = "1CFOE"
    )

    ClThetasImplicit <- StParm(
      StParmName = "Cl",
      Covariates = Covariate(
        Name = "Race",
        Type = "Categorical",
        State = "Searched",
        Categories = c(1, 2, 3)
      ),
      PMLStructure = "1CFOE"
    )

    expect_equal(print(ClThetasExplicit), print(ClThetasImplicit))
  }
)

test_that(
  "Compare output StParm with implicit and explicit thetas for present continuous covariate",
  {
    ClThetasExplicit <- StParm(
      StParmName = "Cl",
      Covariates = Covariate(
        Name = "WT",
        Type = "Continuous",
        State = "Present",
        Center = "Median",
        Thetas = list(Theta(Name = "dCldWT", InitialEstimates = 0))
      ),
      PMLStructure = "1CFOE"
    )

    ClThetasImplicit <- StParm(
      StParmName = "Cl",
      Covariates = Covariate(
        Name = "WT",
        Type = "Continuous",
        State = "Present",
        Center = "Median"
      ),
      PMLStructure = "1CFOE"
    )

    expect_equal(print(ClThetasExplicit), print(ClThetasImplicit))
  }
)

test_that(
  "Compare output StParm with implicit and explicit thetas for present continuous covariate",
  {
    ClThetasExplicit <- StParm(
      StParmName = "Cl",
      Covariates = Covariate(
        Name = "WT",
        Type = "Continuous",
        State = "Searched",
        Thetas = list(Theta(
          Name = "dCldWT",
          InitialEstimates = c(0, 2, 3),
          Frozen = T
        ))
      ),
      PMLStructure = "1CFOE"
    )

    ClThetasImplicit <- StParm(
      StParmName = "Cl",
      Covariates = Covariate(
        Name = "WT",
        Type = "Continuous",
        State = "Searched"
      ),
      PMLStructure = "1CFOE"
    )

    expect_equal(print(ClThetasExplicit), print(ClThetasImplicit))
  }
)
