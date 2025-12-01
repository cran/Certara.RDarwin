test_that("Continuous covariate searched", {
  testthat::skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  DataFilePath <- "{data_dir}/OralBolus.csv"
  DataMapping <-
    c("ID", "TIME", "AMT", "DROP", "SEX", "RACE", "AGE", "BW", "DV")

  NMAdvanMatrix <- advan_matrix

  df_ADVAN <- NMAdvanMatrix %>%
    dplyr::filter(ADVAN %in% c("ADVAN2", "ADVAN4", "ADVAN12")) %>%
    dplyr::filter(TRANS %in% c("TRANS2", "TRANS4"))

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
    ) %>%
    modify_AdvanDF(
      ADVAN = "ADVAN2",
      NMAdvanMatrix,
      Parameter = "V",
      LowerBound = 0,
      InitEst = 5
    ) %>%
    modify_AdvanDF(
      ADVAN = "ADVAN12",
      NMAdvanMatrix,
      Parameter = "V2",
      LowerBound = 0,
      InitEst = 0.1
    ) %>%
    modify_AdvanDF(
      ADVAN = "ADVAN2",
      NMAdvanMatrix    ,
      Parameter = "V",
      ThetaName = "VBW",
      Presence = 1,
      LowerBound = NA,
      InitEst = 1,
      UpperBound = NA,
      OmegaPresence = 0,
      Covariate =  "BW",
      CovarRelation = "power",
      Center = 70
    ) %>%
    modify_AdvanDF(
      ADVAN = c("ADVAN4", "ADVAN12"),
      NMAdvanMatrix,
      Parameter = "V2",
      ThetaName = "V2BW",
      Presence = 2,
      LowerBound = NA,
      InitEst = 1,
      UpperBound = NA,
      OmegaPresence = 0,
      Covariate =  "BW",
      CovarRelation = "power",
      Center = 70
    ) %>%
    modify_AdvanDF(
      ADVAN = c("ADVAN4", "ADVAN12"),
      NMAdvanMatrix,
      Parameter = "V2",
      ThetaName = "V2BW",
      Presence = 2,
      LowerBound = NA,
      InitEst = 0,
      UpperBound = NA,
      OmegaPresence = 0,
      Covariate =  "BW",
      CovarRelation = "linear",
      Center = NA
    )

  Sigmas_Prop <-
    list(
      Additive = 0,
      Proportional = 0.01,
      Combined = c(PropPart = 0, AddPart = 0)
    )

  TemplateFilePath <- file.path(tempdir(), "template.txt")
  TokensFilePath <- file.path(tempdir(), "tokens.json")
  ProblemName = "Test"


  df_Model <- df_ADVAN_Stacked

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
