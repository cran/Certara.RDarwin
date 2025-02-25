test_that("Categorical covariate with multiple relations searched", {
  skip_if_not_installed("dplyr")

  DataFilePath <- "{data_dir}/OralBolus.csv"
  DataMapping <-
    c("ID",
      "TIME",
      "AMT",
      "DROP",
      "SEX",
      "RACE",
      "AGE",
      "BW",
      "DV",
      "MDV")

  NMAdvanMatrix <- advan_matrix

  df_ADVAN <- NMAdvanMatrix %>%
    dplyr::filter(ADVAN == "ADVAN2") %>%
    dplyr::filter(TRANS == "TRANS2")

  df_ADVAN_Stacked <-
    tidyr::pivot_longer(
      df_ADVAN,
      cols = K:dplyr::last_col(),
      names_to = "Parameter",
      values_to = "Presence"
    ) %>%
    dplyr::filter(Presence != 0) %>%
    dplyr::mutate(
      ThetaName = Parameter,
      Covariate = NA,
      CovarRelation = NA,
      Center = NA,
      Category = NA
    ) %>%
    dplyr::mutate(LowerBound = 0,
                  InitEst = 1,
                  UpperBound = NA) %>%
    dplyr::mutate(OmegaPresence = 1,
                  Omega = 1) %>%
    dplyr::mutate(
      ThetaFrozen = 0,
      OmegaFrozen = 0,
      PreCode = NA,
      PostCode = NA
    )

  df_Model_Temp0 <-
    modify_AdvanDF(
      df_ADVAN_Stacked,
      NMAdvanMatrix,
      Parameter = "V",
      LowerBound = 0,
      InitEst = 5
    ) %>%
    modify_AdvanDF(
      NMAdvanMatrix,
      Parameter = "V",
      ThetaName = "VRace1",
      Presence = 2,
      LowerBound = NA,
      InitEst = -1,
      UpperBound = NA,
      OmegaPresence = 0,
      Covariate =  "RACE",
      CovarRelation = "exponential",
      Category = 1
    ) %>%
    modify_AdvanDF(
      NMAdvanMatrix,
      Parameter = "V",
      ThetaName = "VRace2",
      Presence = 2,
      LowerBound = NA,
      InitEst = -2,
      UpperBound = NA,
      OmegaPresence = 0,
      Covariate =  "RACE",
      CovarRelation = "exponential",
      Category = 2
    ) %>%
    modify_AdvanDF(
      NMAdvanMatrix,
      Parameter = "V",
      ThetaName = "VRace1",
      Presence = 2,
      LowerBound = -1,
      InitEst = -0.1,
      UpperBound = 1,
      OmegaPresence = 0,
      Covariate =  "RACE",
      CovarRelation = "linear",
      Category = 1
    ) %>%
    modify_AdvanDF(
      NMAdvanMatrix,
      Parameter = "V",
      ThetaName = "VRace2",
      Presence = 2,
      LowerBound = -1,
      InitEst = -0.2,
      UpperBound = 1,
      OmegaPresence = 0,
      Covariate =  "RACE",
      CovarRelation = "linear",
      Category = 2
    )

  Sigmas_Prop <-
    list(
      Additive = 0,
      Proportional = 0.01,
      Combined = c(PropPart = NA, AddPart = NA)
    )

  TemplateFilePath <- file.path(tempdir(), "template.txt")
  TokensFilePath <- file.path(tempdir(), "tokens.json")
  ProblemName <- "Test"

  df_Model <- df_Model_Temp0

  print_TemplateTokens(
    TemplateFilePath,
    TokensFilePath,
    ModelDF = df_Model,
    ProblemName = ProblemName,
    DataMapping = DataMapping,
    DataFilePath = DataFilePath,
    SigmasChosen = Sigmas_Prop,
    EstimationRow = "METHOD = COND INTER NOABORT MAX = 99",
    CovarianceRow = "UNCOND PRINT=E",
    AppendixRows = ""
  )

  testthat::expect_snapshot_file(TokensFilePath, compare = compare_file_text)
  testthat::expect_snapshot_file(TemplateFilePath, compare = compare_file_text)
})
