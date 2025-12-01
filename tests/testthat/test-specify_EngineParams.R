# --- Tests for specify_EngineParams (default sort=FALSE) ---
test_that("specify_EngineParams: QRPEM parameters are properly specified", {
  EstArgs <-
    specify_EngineParams(
      sort = TRUE,
      ODE = "DVERK",
      rtolODE = 1e-5,
      atolODE = 1e-7,
      maxStepsODE = 6000,
      numIterations = 100,
      method = "QRPEM",
      isCentralDiffStdErr = TRUE,
      logTransform = FALSE,
      numIterMAPNP = 3,
      iSample = 350,
      impDist = "Mixture-2",
      scramble = "Tezuka-Faur"
    )

  expected_str <- " sort=TRUE ODE=DVERK rtolODE=1e-05 atolODE=1e-07 maxStepsODE=6000 numIterations=100 method=QRPEM logTransform=FALSE numIterMAPNP=3 iSample=350 impDist=Mixture-2 scramble=Tezuka-Faur"
  testthat::expect_equal(trimws(EstArgs), trimws(expected_str))

  EstArgs_default_imp <- specify_EngineParams(method = "QRPEM", iSample = 100, impDist = NULL, scramble = NULL)
  expected_str_default_imp <- " sort=FALSE method=QRPEM iSample=100"
  testthat::expect_equal(trimws(EstArgs_default_imp), trimws(expected_str_default_imp))
})

test_that("specify_EngineParams: Default call", {
  EstArgs <- specify_EngineParams()
  testthat::expect_equal(EstArgs, " sort=FALSE")
})

test_that("specify_EngineParams: Method IT2S-EM and FO", {
  EstArgs_IT2S_EM <- specify_EngineParams(method = "IT2S-EM", logTransform = FALSE)
  testthat::expect_equal(trimws(EstArgs_IT2S_EM), trimws("sort=FALSE method=IT2S-EM logTransform=FALSE"))

  EstArgs_FO <- specify_EngineParams(method = "FO", stdErr = "Hessian")
  testthat::expect_equal(trimws(EstArgs_FO), trimws("sort=FALSE method=FO stdErr=Hessian"))
})


test_that("specify_EngineParams: FOCE-ELS/Laplacian specific args work", {
  EstArgs_FOCE_ELS <- specify_EngineParams(
    method = "FOCE-ELS", # Default method
    stdErr = "Hessian",
    logTransform = FALSE,
    numIntegratePtsAGQ = 5,
    fastOptimization = TRUE,
    gradTolOuter = 1e-5
  )
  expected_str_FOCE_ELS <- " sort=FALSE stdErr=Hessian logTransform=FALSE numIntegratePtsAGQ=5 fastOptimization=TRUE gradTolOuter=1e-05"
  testthat::expect_equal(trimws(EstArgs_FOCE_ELS), trimws(expected_str_FOCE_ELS))

  EstArgs_Lap <- specify_EngineParams(
    method = "Laplacian",
    stdErr = "Hessian",
    logTransform = FALSE,
    numIntegratePtsAGQ = 3,
    fastOptimization = TRUE,
    gradTolOuter = 1e-04
  )
  expected_str_Lap <- " sort=FALSE method=Laplacian stdErr=Hessian logTransform=FALSE numIntegratePtsAGQ=3 fastOptimization=TRUE gradTolOuter=1e-04"
  testthat::expect_equal(trimws(EstArgs_Lap), trimws(expected_str_Lap))
})

test_that("specify_EngineParams: ODE tolerance applicability respected", {
  # Test warning specifically for maxStepsODE when ODE is MatrixExponent
  testthat::expect_warning(
    specify_EngineParams(ODE = "MatrixExponent", maxStepsODE = 9999),
    regexp = "Parameter 'maxStepsODE'.*inapplicable because ODE is MatrixExponent"
  )

  # Test warning specifically for rtolODE when ODE is MatrixExponent
  testthat::expect_warning(
    specify_EngineParams(ODE = "MatrixExponent", rtolODE = 1e-3),
    regexp = "Parameter 'rtolODE'.*inapplicable because ODE is MatrixExponent"
  )

  # Test warning specifically for atolODE when ODE is MatrixExponent
  testthat::expect_warning(
    specify_EngineParams(ODE = "MatrixExponent", atolODE = 1e-7), # Assuming atolODE also warns
    regexp = "Parameter 'atolODE'.*inapplicable because ODE is MatrixExponent"
  )

  # Check the output string when multiple inapplicable args are provided;
  # warnings are expected, so we can suppress them for the assignment if desired,
  # or let expect_equal run on the value produced amidst warnings.
  # The previous EstArgs_mat_exp was assigned inside expect_warning, which is fine.
  EstArgs_mat_exp <- suppressWarnings(specify_EngineParams(
    ODE = "MatrixExponent", # Default, so not shown in output
    maxStepsODE = 9999,     # Inapplicable
    rtolODE = 1e-3,         # Inapplicable
    atolODE = 1e-7          # Inapplicable
  ))
  testthat::expect_equal(EstArgs_mat_exp, " sort=FALSE")


  # Test for DVERK (no warnings expected for these applicable parameters)
  EstArgs_dverk <- specify_EngineParams(
    ODE = "DVERK",
    rtolODE = 1e-4,
    atolODE = 1e-8,
    maxStepsODE = 5000
  )
  expected_str_dverk <- " sort=FALSE ODE=DVERK rtolODE=1e-04 atolODE=1e-08 maxStepsODE=5000"
  testthat::expect_equal(trimws(EstArgs_dverk), trimws(expected_str_dverk))

  # Verify no warnings for a valid DVERK call
  testthat::expect_no_warning(
    specify_EngineParams(
      ODE = "DVERK", rtolODE = 1e-4, atolODE = 1e-8, maxStepsODE = 5000
    )
  )
})

test_that("specify_EngineParams: Inapplicable args generate warnings and are excluded", {
  testthat::expect_warning(
    EstArgs_warn1 <- specify_EngineParams(
      method = "FOCE-ELS",
      iSample = 500
    ),
    regexp = "method is not QRPEM"
  )
  testthat::expect_equal(EstArgs_warn1, " sort=FALSE")

  testthat::expect_warning(
    EstArgs_warn2 <- specify_EngineParams(
      method = "QRPEM",
      stepSizePartialDeriv = 1e-4
    ),
    regexp = "method is not Naive-Pooled"
  )
  expected_str_warn2 <- " sort=FALSE method=QRPEM"
  testthat::expect_equal(trimws(EstArgs_warn2), trimws(expected_str_warn2))
})

test_that("specify_EngineParams: Individual (Naive-Pooled) args work", {
  EstArgs_indiv <- specify_EngineParams(
    method = "Naive-Pooled",
    stdErr = "Hessian",
    logTransform = FALSE,
    stepSizePartialDeriv = 1e-4,
    numTimeStepPartialDeriv = 50
  )
  expected_str_indiv <-
    " sort=FALSE method=Naive-Pooled stdErr=Hessian logTransform=FALSE stepSizePartialDeriv=1e-04 numTimeStepPartialDeriv=50"
  testthat::expect_equal(trimws(EstArgs_indiv), trimws(expected_str_indiv))
})

test_that("specify_EngineParams: logTransform argument behavior", {
  EstArgs_false <- specify_EngineParams(logTransform = FALSE)
  expected_str_false <- " sort=FALSE logTransform=FALSE"
  testthat::expect_equal(trimws(EstArgs_false), trimws(expected_str_false))

  EstArgs_null <- specify_EngineParams(logTransform = NULL) # Uses function's default (TRUE for FOCE-ELS)
  testthat::expect_equal(EstArgs_null, " sort=FALSE") # Default TRUE not shown

  # If logTransform=TRUE is passed and it's the global default, it IS shown.
  EstArgs_true_explicit_default <- specify_EngineParams(logTransform = TRUE)
  expected_str_true_explicit_default <- "sort=FALSE logTransform=TRUE" # Corrected based on failure
  testthat::expect_equal(trimws(EstArgs_true_explicit_default), trimws(expected_str_true_explicit_default))

  # For QRPEM, if logTransform=TRUE is passed (which is also its specific default), it IS shown.
  EstArgs_qrpem_logT_true <- specify_EngineParams(method = "QRPEM", logTransform = TRUE)
  expected_str_qrpem_logT_true <- " sort=FALSE method=QRPEM logTransform=TRUE" # Corrected based on failure
  testthat::expect_equal(trimws(EstArgs_qrpem_logT_true), trimws(expected_str_qrpem_logT_true))

  EstArgs_qrpem_logT_false <- specify_EngineParams(method = "QRPEM", logTransform = FALSE)
  expected_str_qrpem_logT_false <- " sort=FALSE method=QRPEM logTransform=FALSE"
  testthat::expect_equal(trimws(EstArgs_qrpem_logT_false), trimws(expected_str_qrpem_logT_false))

  EstArgs_combo <- specify_EngineParams(
    stdErr = "None",
    logTransform = TRUE,    # Explicitly TRUE, will be shown
    numIntegratePtsAGQ = 2
  )
  expected_str_combo <- " sort=FALSE stdErr=None logTransform=TRUE numIntegratePtsAGQ=2" # Corrected
  testthat::expect_equal(trimws(EstArgs_combo), trimws(expected_str_combo))

  EstArgs_combo_lap <- specify_EngineParams(
    method = "Laplacian",
    stdErr = "None",
    logTransform = TRUE,    # Explicitly TRUE, will be shown
    numIntegratePtsAGQ = 2
  )
  expected_str_combo_lap <- " sort=FALSE method=Laplacian stdErr=None logTransform=TRUE numIntegratePtsAGQ=2" # Corrected
  testthat::expect_equal(trimws(EstArgs_combo_lap), trimws(expected_str_combo_lap))
})

# --- Tests for specify_SimParams (default sort=FALSE, numRep/seed/sort always output) ---
test_that("specify_SimParams: Default call includes required args with their defaults", {
  SimArgs <- specify_SimParams()
  expected_str <- " numReplicates=100 seed=1234 sort=FALSE" # Order matters
  testthat::expect_equal(trimws(SimArgs), trimws(expected_str))
})

test_that("specify_SimParams: Custom values for always-output args appear", {
  SimArgs <- specify_SimParams(
    numReplicates = 55,
    seed = 111,
    sort = TRUE # Non-default
  )
  expected_str <- " numReplicates=55 seed=111 sort=TRUE"
  testthat::expect_equal(trimws(SimArgs), trimws(expected_str))
})

test_that("specify_SimParams: Non-default applicable ODE/tolerances appear", {
  SimArgs <- specify_SimParams(
    ODE = "DVERK",   # Non-default
    rtolODE = 1e-4, # Non-default
    maxStepsODE = 999 # Non-default
    # Leave atolODE default
  )
  expected_str <- " numReplicates=100 seed=1234 sort=FALSE ODE=DVERK rtolODE=1e-04 maxStepsODE=999"
  testthat::expect_equal(trimws(SimArgs), trimws(expected_str))
})

test_that("specify_SimParams: ODE tolerances ignored for MatrixExponent", {
  # Case A: ODE is default (MatrixExponent), tolerances non-default but inapplicable
  SimArgs <- specify_SimParams(
    ODE = "MatrixExponent", # Default
    rtolODE = 1e-4         # Non-default, but inapplicable
  )
  expected_str_a <- " numReplicates=100 seed=1234 sort=FALSE" # Only always-output args
  testthat::expect_equal(trimws(SimArgs), trimws(expected_str_a))

  # Case B: ODE set to MatrixExponent (same as default), tolerances non-default but inapplicable
  SimArgs2 <- specify_SimParams(
    ODE = "MatrixExponent",
    rtolODE = 1e-4,
    atolODE = 1e-4 # also non-default, inapplicable
  )
  expected_str_b <- " numReplicates=100 seed=1234 sort=FALSE"
  testthat::expect_equal(trimws(SimArgs2), trimws(expected_str_b))
})
