test_that("2 Categories categorical covariate searched", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  DataFilePath <- "{data_dir}/OralBolus.csv"
  DataMapping <-
    c("ID", "TIME", "AMT", "DROP", "SEX", "RACE", "AGE", "BW", "DV")
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
    )

  df_Model_Sex1_2 <- modify_AdvanDF(
    df_Model_Temp0,
    NMAdvanMatrix,
    Parameter = "V",
    ThetaName = "VSex1",
    Presence = 2,
    LowerBound = -1,
    InitEst = -0.1,
    UpperBound = NA,
    OmegaPresence = 0,
    Covariate =  "SEX",
    CovarRelation = "linear",
    Category = 1
  )

  Sigmas_Prop_Combined <-
    list(
      Additive = 0,
      Proportional = 0.01,
      Combined = c(PropPart = 0.01, AddPart = 0.01)
    )

  TemplateFilePath <- file.path(tempdir(), "template.txt")
  TokensFilePath <- file.path(tempdir(), "tokens.json")
  ProblemName = "Test"

  df_Model <- df_Model_Sex1_2

  print_TemplateTokens(
    TemplateFilePath,
    TokensFilePath,
    ModelDF = df_Model,
    ProblemName = ProblemName,
    DataMapping = DataMapping,
    DataFilePath = DataFilePath,
    SigmasChosen = Sigmas_Prop_Combined,
    EstimationRow = "METHOD = COND INTER NOABORT MAX = 99",
    CovarianceRow = "UNCOND PRINT=E",
    AppendixRows = ""
  )

  testthat::expect_snapshot_file(TokensFilePath, compare = compare_file_text)
  testthat::expect_snapshot_file(TemplateFilePath, compare = compare_file_text)
})
