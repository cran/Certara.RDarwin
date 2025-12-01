test_that("output_NLMETemplate generates template and tokens from valid arguments",
          {
            # 1. Setup: Create all necessary files in a temporary directory
            TestFolder <- tempdir()
            ModelSetupPath <- file.path(TestFolder, "test_setup.json")
            TemplateFilePath <- file.path(TestFolder, "test_template.txt")
            TokensFilePath <- file.path(TestFolder, "test_tokens.json")
            DataFilePath <- file.path(TestFolder, "test_data.csv")

            # Create a valid JSON model setup file.
            # OmegaState for Cl and V is now set to "Present", which is a valid state.
            model_setup_list <- list(
              ModelSetup = list(
                Type = "PK",
                Infusion = "None",
                ElimCptPresence = FALSE,
                ElimCptReset = FALSE,
                Covariates = list(
                  Continuous = list(),
                  Categorical = list(),
                  Occasional = list()
                ),
                Mapping = list(
                  id = "ID",
                  time = "TIME",
                  AMT = "AMT",
                  CObs = "DV"
                ),
                Parameterization = "Clearance",
                Administration = "Intravenous",
                Compartments = list(`1-Compartment` = list(Elimination = "Linear")),
                Observations = list(CObs = list(Error = list(
                  Proportional = list(Initial = 0.1, Freeze = FALSE)
                ))),
                StParms = list(
                  Cl = list(Type = "LogNormal", OmegaState = "Present"),
                  V = list(Type = "LogNormal", OmegaState = "Present")
                ),
                CovariateEffects = list(),
                FixedEffects = list(tvCl = list(Initial = 1), tvV = list(Initial = 10)),
                RandomEffects = list(nCl = list(Initial = 0.09), nV = list(Initial = 0.09))
              ),
              EngineSetup = list(
                ESTARGS = "",
                SIMARGS = "",
                COLDEFS = ""
              )
            )
            jsonlite::write_json(model_setup_list, ModelSetupPath, auto_unbox = TRUE)

            # Create a data file with columns matching the mapping in the JSON
            write.csv(data.frame(
              ID = 1,
              TIME = c(0, 1),
              AMT = c(100, 0),
              DV = c(NA, 10)
            ),
            DataFilePath,
            row.names = FALSE)

            # 2. Action: Construct the arguments and run the function
            args <- c(
              paste0("model_setup=", ModelSetupPath),
              paste0("template_path=", TemplateFilePath),
              paste0("tokens_path=", TokensFilePath),
              paste0("data_path=", DataFilePath),
              "author=R Test Suite",
              "description=A basic 1-cpt PK model test"
            )

            output_NLMETemplate(args)

            # 3. Assert: Check for file existence and snapshot the contents
            testthat::local_edition(3)
            testthat::expect_true(file.exists(TemplateFilePath))

            testthat::expect_snapshot_file(TemplateFilePath, "template_from_args.txt")
          })

test_that(
  "output_NLMETemplate handles a 2-cpt model with covariates and first-order absorption",
  {
    # 1. Setup: Create all necessary files for a more complex scenario
    TestFolder <- tempdir()
    ModelSetupPath <- file.path(TestFolder, "complex_setup.json")
    TemplateFilePath <- file.path(TestFolder, "complex_template.txt")
    TokensFilePath <- file.path(TestFolder, "complex_tokens.json")
    DataFilePath <- file.path(TestFolder, "complex_data.csv")

    # Create a more detailed JSON model setup file
    model_setup_list <- list(
      ModelSetup = list(
        Type = "PK",
        Infusion = "None",
        ElimCptPresence = FALSE,
        ElimCptReset = FALSE,
        # Define a continuous covariate
        Covariates = list(
          Continuous = list(WT = list(
            Center = 70, Direction = "Forward"
          )),
          Categorical = list(),
          Occasional = list()
        ),
        # Map to custom column names in the data file
        Mapping = list(
          id = "SubjectID",
          time = "Time_h",
          AMT = "Dose_mg",
          CObs = "Conc_ng_mL",
          WT = "WT"
        ),
        Parameterization = "Clearance",
        Administration = "Extravascular",
        Compartments = list(
          `2-Compartment` = list(Absorption = "First-Order", Elimination = "Linear")
        ),
        # Use a combined error model
        Observations = list(CObs = list(Error = list(
          AdditiveMultiplicative = list(Initial = 0.1, Freeze = FALSE)
        ))),
        StParms = list(
          Ka = list(Type = "LogNormal", OmegaState = "Present"),
          Cl = list(Type = "LogNormal", OmegaState = "Searched"),
          # Use "Searched" state
          V = list(Type = "LogNormal", OmegaState = "Present"),
          Cl2 = list(Type = "LogNormal", OmegaState = "None"),
          V2 = list(Type = "LogNormal", OmegaState = "None")
        ),
        # Define the effect of the covariate on Clearance
        CovariateEffects = list(Cl = list(WT = "Present")),
        FixedEffects = list(
          tvKa = list(Initial = c(1, 2)),
          tvCl = list(Initial = 5),
          tvV = list(Initial = 50),
          tvCl2 = list(Initial = 2, Freeze = TRUE),
          tvV2 = list(Initial = 100, Freeze = TRUE)
        ),
        RandomEffects = list(
          nKa = list(Initial = 0.09),
          nCl = list(Initial = 0.09),
          nV = list(Initial = 0.09)
        )
      ),
      EngineSetup = list(
        ESTARGS = "maxiter=1000",
        SIMARGS = "",
        COLDEFS = ""
      )
    )
    jsonlite::write_json(model_setup_list, ModelSetupPath, auto_unbox = TRUE)

    # Create a data file with the custom column names
    write.csv(
      data.frame(
        SubjectID = 1,
        Time_h = c(0, 2, 4, 8),
        Dose_mg = c(500, 0, 0, 0),
        Conc_ng_mL = c(NA, 45, 60, 35),
        WT = 75
      ),
      DataFilePath,
      row.names = FALSE
    )

    # 2. Action: Construct the arguments vector and run the function
    args <- c(
      paste0("model_setup=", ModelSetupPath),
      paste0("template_path=", TemplateFilePath),
      paste0("tokens_path=", TokensFilePath),
      paste0("data_path=", DataFilePath),
      "author=R Test Suite",
      "description=A 2-cpt PK model with a covariate"
    )

    suppressMessages(output_NLMETemplate(args))

    # 3. Assert: Check for file existence and snapshot their content to verify correctness
    testthat::local_edition(3)
    testthat::expect_true(file.exists(TemplateFilePath))
    testthat::expect_true(file.exists(TokensFilePath))

    testthat::expect_snapshot_file(TemplateFilePath, "complex_template_from_args.txt")
  }
)
