test_that("THETA(D1) searched ETA(D1) searched", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  Rate <- TRUE
  DataFilePath <- "{data_dir}/OralBolusRD.csv"

  if (Rate) {
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
        "RATE",
        "DROP")
  } else {
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
        "DROP",
        "RATE")
  }

  NMAdvanMatrix <- advan_matrix
  df_ADVAN <- NMAdvanMatrix %>%
    dplyr::filter(ADVAN %in% c("ADVAN2")) %>%
    dplyr::filter(TRANS %in% c("TRANS2"))

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

  df_Model_Temp0 <- modify_AdvanDF(
    df_ADVAN_Stacked,
    NMAdvanMatrix,
    Parameter = "V",
    LowerBound = 0,
    InitEst = 5
  )


  df_ADVAN2_Rate <- modify_AdvanDF(
    df_Model_Temp0,
    NMAdvanMatrix,
    ADVAN = "ADVAN2",
    Parameter = "RATE",
    Presence = 2,
    LowerBound = NA,
    InitEst = NA,
    Omega = NA,
    OmegaPresence = 2,
    ThetaFrozen = 0,
    OmegaFrozen = 0
  )

  if (Rate) {
    df_ADVAN2_Rate <- modify_AdvanDF(
      df_ADVAN2_Rate,
      NMAdvanMatrix,
      ADVAN = "ADVAN2",
      Parameter = "R1",
      Presence = 2,
      LowerBound = 0,
      InitEst = 50,
      OmegaPresence = 2,
      Omega = 0.09,
      ThetaFrozen = 0,
      OmegaFrozen = 0
    )
  } else {
    df_ADVAN2_Rate <- modify_AdvanDF(
      df_ADVAN2_Rate,
      NMAdvanMatrix,
      ADVAN = "ADVAN2",
      Parameter = "D1",
      Presence = 2,
      LowerBound = 0,
      InitEst = 2,
      OmegaPresence = 2,
      Omega = 0.09,
      ThetaFrozen = 0,
      OmegaFrozen = 0
    )
  }

  Sigmas_Prop <-
    list(
      Additive = 0,
      Proportional = 0.01,
      Combined = c(PropPart = 0, AddPart = 0)
    )

  TemplateFilePath <- file.path(tempdir(), "template.txt")
  TokensFilePath <- file.path(tempdir(), "tokens.json")
  ProblemName = "Test"

  df_Model <- df_ADVAN2_Rate

  print_TemplateTokens(
    TemplateFilePath,
    TokensFilePath,
    ModelDF = df_Model,
    ProblemName = ProblemName,
    DataMapping = DataMapping,
    DataFilePath = DataFilePath,
    SigmasChosen = Sigmas_Prop,
    EstimationRow = "METHOD = COND INTER NOABORT MAX = 99 PRINT = 5",
    CovarianceRow = "UNCOND PRINT=E",
    AppendixRows = ""
  )

  testthat::expect_snapshot_file(TokensFilePath, compare = compare_file_text)
  testthat::expect_snapshot_file(TemplateFilePath, compare = compare_file_text)
})
