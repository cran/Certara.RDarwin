test_that("Modify Theta parameters in PMLParametersSets", {
  # Load your modify_Theta function here (if not already loaded)
  # Load other necessary functions and data structures if not already loaded

  # Define a helper function to check if two Theta instances are equal
  check_theta_equal <- function(theta1, theta2) {
    return(
      identical(theta1$Name, theta2$Name) &&
        identical(theta1$InitialEstimates, theta2$InitialEstimates) &&
        identical(theta1$State, theta2$State) &&
        identical(theta1$Frozen, theta2$Frozen) &&
        identical(theta1$StParmName, theta2$StParmName)
    )
  }

  # Create a sample PMLParametersSets (replace with your data if necessary)
  PMLParametersSets <-
    get_PMLParametersSets(Absorption = "First-Order",
                          Saturation = c(TRUE, FALSE))

  # Modify an existing Theta
  modified_PMLParametersSets <- modify_Theta(PMLParametersSets,
                                             Name = "tvV",
                                             InitialEstimates = c(0.5, 0.6, 0.7))

  # Retrieve the modified Theta from the modified PMLParametersSets
  modified_theta <- .get_ClassInstance(modified_PMLParametersSets,
                                       Name = "tvV",
                                       InstanceNameElement = "Name")

  # Define the expected modified Theta
  expected_theta <- Theta(
    Name = "tvV",
    InitialEstimates = c(0.5, 0.6, 0.7),
    State = "Present",
    Frozen = FALSE,
  )

  # Check if the modified Theta matches the expected Theta
  expect_true(check_theta_equal(modified_theta, expected_theta))

  expect_warning(modify_Theta(PMLParametersSets, Name = "tvC", Frozen = TRUE))
})
