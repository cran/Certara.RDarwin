test_that(
  "Dosepoint: Set tlag(Searched), duration(StParm/Expression with StParm), Omega searched",
  {
    TestFolder <- tempdir()

    modelPMLCodes <- create_ModelPK(
      CompartmentsNumber = 1,
      Parameterization = "Clearance",
      Saturation = c(TRUE, FALSE),
      Absorption = "First-Order",
      EliminationCpt = c(TRUE, FALSE)
    )

    modelPMLCodes <-
      modify_Dosepoint(
        modelPMLCodes,
        DosepointName = "Aa",
        tlag = StParm("Tlag", State = "Searched"),
        PMLStructures = c("PK1FOC", "PK1FOCSE")
      )

    # Modify observation for one structure
    modelPMLCodes <-
      modify_Observation(
        modelPMLCodes,
        ObservationName = "CObs",
        ResetObs = FALSE,
        PMLStructures = "PK1FOCSE"
      )

    # Modify Omega state for nV in one structure
    modelPMLCodes <-
      modify_Omega(
        modelPMLCodes,
        Name = "nV",
        # Assumes nV exists from create_ModelPK
        State = "Searched",
        PMLStructures = c("PK1FOCSE")
      )

    # Modify duration using StParm("D") for PK1FOCSE
    modelPMLCodes <-
      modify_Dosepoint(
        modelPMLCodes,
        DosepointName = "Aa",
        duration = StParm("D"),
        PMLStructures = c("PK1FOCSE")
      )

    # Define the StParm for Rate needed in the expression for PK1FOC
    RateStParm <- StParm(
      StParmName = "Rate",
      OmegaStParm = Omega(Name = "nRate", State = "None"),
      # Provide Name even if State is None
      ThetaStParm = Theta(Name = "tvRate", InitialEstimates = InitialEstimate(c(0, 1, Inf))) # Provide Name
    )

    # Modify duration using Expression("1/Rate") for PK1FOC
    modelPMLCodes <-
      modify_Dosepoint(
        modelPMLCodes,
        DosepointName = "Aa",
        duration = Expression("1/Rate",
                              ContainedStParms = list(RateStParm)),
        PMLStructures = "PK1FOC"
      )

    # --- Verification ---
    testthat::expect_s3_class(modelPMLCodes$PK1FOC$MainDosepoint$Aa$duration,
                              "Expression")
    testthat::expect_equal(modelPMLCodes$PK1FOC$MainDosepoint$Aa$duration$ExpressionText,
                           "1/Rate")
    testthat::expect_equal(length(
      modelPMLCodes$PK1FOC$MainDosepoint$Aa$duration$ContainedStParms
    ),
    1)
    testthat::expect_equal(
      modelPMLCodes$PK1FOC$MainDosepoint$Aa$duration$ContainedStParms[[1]]$StParmName,
      "Rate"
    )
    # Check the Theta/Omega names within RateStParm
    testthat::expect_equal(
      modelPMLCodes$PK1FOC$MainDosepoint$Aa$duration$ContainedStParms[[1]]$ThetaStParm$Name,
      "tvRate"
    )
    testthat::expect_equal(
      modelPMLCodes$PK1FOC$MainDosepoint$Aa$duration$ContainedStParms[[1]]$OmegaStParm$Name,
      "nRate"
    )
    testthat::expect_equal(
      modelPMLCodes$PK1FOC$MainDosepoint$Aa$duration$ContainedStParms[[1]]$OmegaStParm$State,
      "None"
    )
    testthat::expect_equal(
      modelPMLCodes$PK1FOC$MainDosepoint$Aa$duration$ContainedStParms[[1]]$ThetaStParm$InitialEstimates$Lower,
      0
    )

    testthat::expect_s3_class(modelPMLCodes$PK1FOCSE$MainDosepoint$Aa$duration,
                              "StParm")
    testthat::expect_equal(modelPMLCodes$PK1FOCSE$MainDosepoint$Aa$duration$StParmName,
                           "D")
    # Check implicitly created names using the REAL constructors
    testthat::expect_equal(modelPMLCodes$PK1FOCSE$MainDosepoint$Aa$duration$ThetaStParm$Name,
                           "tvD")
    testthat::expect_equal(modelPMLCodes$PK1FOCSE$MainDosepoint$Aa$duration$OmegaStParm$Name,
                           "nD")

    testthat::expect_s3_class(modelPMLCodes$PK1FOC$MainDosepoint$Aa$tlag, "StParm")
    testthat::expect_equal(modelPMLCodes$PK1FOC$MainDosepoint$Aa$tlag$State,
                           "Searched")
    # Check implicitly created names using the REAL constructors
    testthat::expect_equal(modelPMLCodes$PK1FOC$MainDosepoint$Aa$tlag$ThetaStParm$Name,
                           "tvTlag")
    testthat::expect_equal(modelPMLCodes$PK1FOC$MainDosepoint$Aa$tlag$OmegaStParm$Name,
                           "nTlag")

    testthat::expect_s3_class(modelPMLCodes$PK1FOCSE$MainDosepoint$Aa$tlag, "StParm")
    testthat::expect_equal(modelPMLCodes$PK1FOCSE$MainDosepoint$Aa$tlag$State,
                           "Searched")

    testthat::expect_equal(modelPMLCodes$PK1FOCSE$StParms$V$OmegaStParm$State,
                           "Searched") # Assumes V StParm exists


    # --- Snapshot Generation ---
    TemplateFilePath <-
      file.path(TestFolder, "template_test1_v4.txt") # Incremented version
    TokensFilePath <-
      file.path(TestFolder, "tokens_test1_v4.json")   # Incremented version
    DataFilePath <-
      file.path(TestFolder, "Data_test1_v4.csv")     # Incremented version
    write.csv(
      data.frame(
        ID = "ID",
        time = "time",
        AaDose = "AaDose",
        CObs = "CObs"
      ),
      DataFilePath,
      row.names = FALSE
    )

    output <-
      write_ModelTemplateTokens(
        TemplateFilePath = TemplateFilePath,
        TokensFilePath = TokensFilePath,
        Description = "Test1v4: Search Tlag, diff durations(Expr/StParm), search nV",
        Author = "Certara",
        DataFilePath = DataFilePath,
        DataMapping = c(
          ID = "ID",
          time = "time",
          Aa = "AaDose",
          CObs = "CObs"
        ),
        PMLParametersSets = modelPMLCodes,
        OmegaSearchBlocks = list(c("nKa", "nV", "nD", "nTlag")) # nRate has State=None, so shouldn't be in search block
      )

    output$Template$DATA <- NULL
    testthat::expect_snapshot_output(cat(unlist(output$Template), sep = "\n"))
    testthat::expect_snapshot_value(output$TokensList, style = "json2")
  }
)

test_that("Covariates searched on Cl and V: Sex",
          {
            TestFolder <- tempdir()

            modelPMLCodes <-
              get_PMLParametersSets(CompartmentsNumber = 2,
                                    Parameterization = "Clearance")

            modelPMLCodes <-
              add_Covariate(
                modelPMLCodes,
                Name = "Sex",
                Type = "Categorical",
                StParmNames = c("Cl", "V"),
                State = "Searched",
                Categories = c(0, 1)
              )

            TemplateFilePath <-
              file.path(TestFolder, "template.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokensSex.json")
            DataFilePath <-
              file.path(TestFolder, "pkData.csv")
            write.csv(
              data.frame(
                subject = 'subject',
                Act_Time = 'Act_Time',
                Amount = 'Amount',
                Conc = 'Conc',
                Age = 'Age',
                BodyWeight = 'BodyWeight',
                Gender = 'Gender'
              ),
              DataFilePath
            )

            output <- write_ModelTemplateTokens(
              TemplateFilePath = TemplateFilePath,
              TokensFilePath = TokensFilePath,
              Description = "SearchCovariates",
              Author = "Certara",
              DataFilePath = DataFilePath,
              DataMapping = c(
                id = "subject",
                time = "Act_Time",
                A1 = "Amount",
                CObs = "Conc",
                Age = "Age",
                BW = "BodyWeight",
                Sex = "Gender(female = 0, male = 1)"
              ),
              PMLParametersSets = modelPMLCodes,
              EstArgs = "",
              OmegaSearchBlocks = list(c("nCl2", "nCl"), c("nV2", "nV"))
            )

            output$Template$DATA <- NULL
            testthat::expect_snapshot_output(cat(unlist(output$Template), sep = "\n"))
            testthat::expect_snapshot_value(output$TokensList, style = "json2")
          })

test_that("Absorption types: Zero-Order, Gaussian, Inverse Gaussian, and Weibull ",
          {
            TestFolder <- tempdir()

            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Saturation = TRUE,
                Absorption = c("Intravenous", "Inverse Gaussian", "Gamma", "Weibull")
              )

            # modelPMLCodes$PK1IVCS$MainDosepoint$A1$duration <-
            #   StParm("D", State = "Searched")
            modelPMLCodes <-
              modify_Dosepoint(
                modelPMLCodes,
                DosepointName = "A1",
                duration = StParm("D", State = "Searched"),
                PMLStructures = "PK1IVCS"
              )

            TemplateFilePath <-
              file.path(TestFolder, "template.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokensZOGIGW.json")
            DataFilePath <-
              file.path(TestFolder, "pkData.csv")
            write.csv(data.frame(
              ID = "ID",
              time = "time",
              Dose = "Dose",
              CObs = "CObs"
            ),
            DataFilePath)

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "SearchAbsorption",
                Author = "Certara",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  ID = "ID",
                  time = "time",
                  A1 = "Dose",
                  CObs = "CObs"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = specify_EngineParams(method = "QRPEM"),
                SimArgs = specify_SimParams(numReplicates = 200, seed = 1),
                AppendixRows = ""
              )

            output$Template$DATA <- NULL
            testthat::expect_snapshot_output(cat(unlist(output$Template), sep = "\n"))
            testthat::expect_snapshot_value(output$TokensList, style = "json2")
          })

test_that("RESE: AdditiveMultiplicative, MixRatio ",
          {
            TestFolder <- tempdir()

            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 2,
                Parameterization = "Clearance",
                CObs = Observation(
                  "CObs",
                  SigmasChosen = list(
                    AdditiveMultiplicative = c(PropPart = 0.1, AddPart = 0.01),
                    MixRatio = c(PropPart = 10, AddPart = 0.01)
                  ),
                  Frozen = TRUE
                )
              )

            TemplateFilePath <-
              file.path(TestFolder, "template.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokensMixRatio.json")
            DataFilePath <-
              file.path(TestFolder, "TwoCpt_IVBolus_PlasmaUrineObs.csv")
            write.csv(data.frame(
              ID = "ID",
              time = "time",
              Dose = "Dose",
              CObs = "CObs"
            ),
            DataFilePath)

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "SearchRSE_enableRanefNewStparm",
                Author = "Certara",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  ID = "ID",
                  time = "time",
                  A1 = "Dose",
                  CObs = "CObs"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = specify_EngineParams(ODE = "DVERK")
              )

            output$Template$DATA <- NULL
            testthat::expect_snapshot_output(cat(unlist(output$Template), sep = "\n"))
            testthat::expect_snapshot_value(output$TokensList, style = "json2")
          })

test_that("Covariates searched ",
          {
            TestFolder <- tempdir()

            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 2,
                Parameterization = "Clearance",
                Age = Covariate(
                  Name = "Age",
                  StParmName = "Cl",
                  State = "Searched",
                  Center = "Median"
                ),
                CObs = Observation(
                  ObservationName = "CObs",
                  SigmasChosen = list(Power = c(Stdev = 1, Power = 0.5)),
                  BQL = TRUE,
                  BQLValue = 22
                ),
                BW = Covariate(
                  Name = "BW",
                  StParmName = "Cl",
                  State = "Searched",
                  Center = 30
                ),
                Sex = Covariate(
                  Name = "Sex",
                  StParmName = "Cl",
                  Type = "Categorical",
                  State = "Searched",
                  Categories = c(0, 1)
                ),
                Occasion = Covariate(
                  Name = "Occasion",
                  StParmName = "V",
                  Type = "Occasion",
                  State = "Searched",
                  Categories = c(1, 2, 3)
                )
              )

            TemplateFilePath <-
              file.path(TestFolder, "template.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokensCovs.json")
            DataFilePath <-
              file.path(TestFolder, "TwoCpt_IVBolus_PlasmaUrineObs.csv")
            write.csv(
              data.frame(
                ID = 'ID',
                time = 'time',
                Dose = 'Dose',
                Conc = 'Conc',
                Age = 'Age',
                BodyWeight = 'BodyWeight',
                Gender = 'Gender',
                Occasion = 'Occasion'
              ),
              DataFilePath
            )

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "SearchCovariates",
                Author = "Certara",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  ID = "ID",
                  time = "time",
                  A1 = "Dose",
                  CObs = "Conc",
                  Age = "Age",
                  BW = "BodyWeight",
                  Sex = "Gender",
                  Occasion = "Occasion"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = specify_EngineParams(
                  method = "QRPEM",
                  ODE = "AutoDetect",
                  numSampleSIR = 15L,
                  numBurnIn = 1L,
                  freezeOmega = FALSE,
                  MCPEM = TRUE
                )
              )

            output$Template$DATA <- NULL
            testthat::expect_snapshot_output(cat(unlist(output$Template), sep = "\n"))
            testthat::expect_snapshot_value(output$TokensList, style = "json2")
          })

test_that("Covariates searched ",
          {
            TestFolder <- tempdir()

            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Absorption = c("First-Order", "Weibull"),
                ByVector = FALSE,
                Cl = StParm(
                  StParmName = "Cl",
                  Type = "LogNormal2",
                  ThetaStParm = Theta(Name = "tvCl",
                                      InitialEstimates = InitialEstimate(c(-Inf, 0.2, Inf),
                                                                         c(0, 3, 10)))
                ),
                tlag = StParm(
                  StParmName = "Tlag",
                  State = "Searched",
                  PMLStructure = "PK1FOC",
                  Covariates = list(
                    Age = Covariate(
                      Name = "Age",
                      Type = "Categorical",
                      State = "Searched",
                      Direction = "Backward",
                      Center = "None",
                      Categories = c(1, 2, 3)
                    )
                  )
                ),
                tvKa = Theta(Name = "tvKa", InitialEstimates = 10),
                nV = Omega(Name = "nV", InitialOmega = 0.1),
                CObs = Observation(
                  ObservationName = "CObs",
                  SigmasChosen = list(
                    AdditiveMultiplicative = c(PropPart = 0.1, AddPart = 2),
                    Proportional = 1
                  ),
                  BQL = TRUE
                ),
                A1 = Dosepoint(
                  DosepointName = "A1",
                  rate = StParm(StParmName = "Rate"),
                  PMLStructure = "PK1WC"
                ),
                Weight = Covariate(
                  Name = "Weight",
                  State = "Searched",
                  Center = "Median"
                )
              )

            TemplateFilePath <-
              file.path(TestFolder, "templateEllipsis.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokensEllipsis.json")
            DataFilePath <-
              file.path(TestFolder, "TwoCpt_IVBolus_PlasmaUrineObs.csv")
            write.csv(
              data.frame(
                ID = 'ID',
                time = 'time',
                AMT = 'AMT',
                Conc = 'Conc',
                Age = 'Age',
                Weight = 'Weight',
                CObsBQL = 'CObsBQL'
              ),
              DataFilePath
            )

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Ellipsis",
                Author = "Certara",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  ID = "ID",
                  time = "time",
                  AMT = "AMT",
                  CObs = "Conc",
                  Age = "Age",
                  Weight = "Weight",
                  CObsBQL = 'CObsBQL'
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = specify_EngineParams(
                  method = "FOCE-LB",
                  stdErr = "Auto-Detect",
                  numIterations = 30,
                  numIterNonParametric = 3
                )
              )

            output$Template$DATA <- NULL
            testthat::expect_snapshot_output(cat(unlist(output$Template), sep = "\n"))
            testthat::expect_snapshot_value(output$TokensList, style = "json2")
          })

test_that("Warning related to interpolated Covariates ",
          {
            TestFolder <- tempdir()

            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                CObs = Observation(
                  ObservationName = "CObs",
                  SigmasChosen = list(Additive = 0.02),
                  BQL = TRUE,
                  BQLValue = 0.1
                ),
                V = StParm(StParmName = "V", Type = "LogNormal2"),
                Cl = StParm(StParmName = "Cl", Type = "LogNormal2"),
                scr = Covariate(
                  Name = "scr",
                  Direction = "Interpolate",
                  State = "Searched"
                )
              )

            TemplateFilePath <-
              file.path(TestFolder, "templateEllipsis.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokensEllipsis.json")
            DataFilePath <-
              file.path(TestFolder, "TwoCpt_IVBolus_PlasmaUrineObs.csv")
            write.csv(data.frame(
              id = "id",
              time = "time",
              dose = "dose",
              dv = "dv",
              scr = "scr"
            ),
            DataFilePath)

            testthat::expect_warning(
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "searchCov",
                Author = "Certara",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "id",
                  time = "time",
                  AMT = "dose",
                  CObs = "dv",
                  scr = "scr"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = specify_EngineParams(
                  sort = TRUE,
                  stdErr = "Hessian",
                  isCentralDiffStdErr = TRUE,
                  stepSizeStdErr = 0.1,
                  numIntegratePtsAGQ = 2L
                ),
                SimArgs = specify_SimParams(
                  numReplicates = 20,
                  sort = TRUE,
                  rtolODE = 1e-5
                )
              )
            )
          })

test_that("StParms in Sigmas correctly named ",
          {
            TestFolder <- tempdir()

            RSE_CObs <-
              Observation(SigmasChosen =
                            Sigmas(AdditiveMultiplicative =
                                     list(
                                       PropPart = 0.1, AddPart = 0.01
                                     )))

            RSE_A0Obs <- Observation(
              ObservationName = "A0Obs",
              SigmasChosen =
                Sigmas(AdditiveMultiplicative =
                         list(
                           PropPart = 0.2, AddPart = 0.02
                         )),
              ResetObs = TRUE
            )

            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = c(1, 2, 3),
                Parameterization = "Clearance",
                Saturation = c(TRUE, FALSE),
                EliminationCpt = TRUE,
                ByVector = FALSE,
                CObs = RSE_CObs,
                A0Obs = RSE_A0Obs
              )

            TemplateFilePath <-
              file.path(TestFolder, "template.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens.json")
            DataFilePath <-
              file.path(TestFolder, "TwoCpt_IVBolus_PlasmaUrineObs.csv")
            write.csv(data.frame(
              SubID = "SubID",
              time = "time",
              Dose = "Dose",
              CObs = "CObs"
            ),
            DataFilePath)

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "SearchNumCptElimTypeRSE",
                Author = "Certara",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "SubID",
                  time = "time",
                  A1 = "Dose",
                  CObs = "CObs"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = specify_EngineParams(numIterations = 10)
              )


            output$Template$DATA <- NULL
            testthat::expect_snapshot_output(cat(unlist(output$Template), sep = "\n"))
            testthat::expect_snapshot_value(output$TokensList, style = "json2")
          })

test_that("Dosepoint: Expression class usage (text, multi-StParm, math, states)",
          {
            TestFolder <- tempdir()

            modelPMLCodes <- create_ModelPK(
              CompartmentsNumber = 1,
              Parameterization = "Clearance",
              Absorption = "First-Order" # Gives PK1FOC with dosepoint Aa
            )

            # Define StParms needed for expressions
            F1_stparm <-
              StParm(
                StParmName = "F1",
                ThetaStParm = Theta(Name = "tvF1", InitialEstimates = 0.8)
              )
            F2_stparm <-
              StParm(
                StParmName = "F2",
                ThetaStParm = Theta(Name = "tvF2", InitialEstimates = 0.9)
              )
            BaseRate_stparm <-
              StParm(
                StParmName = "BaseRate",
                ThetaStParm = Theta(Name = "tvBaseRate", InitialEstimates = 5)
              )

            # Modify the 'Aa' dosepoint using various Expressions in one call
            modelPMLCodes <-
              modify_Dosepoint(
                PMLParametersSets = modelPMLCodes,
                DosepointName = "Aa",
                PMLStructures = "PK1FOC",
                # Apply only to this structure

                # 1. tlag: Simple text expression
                tlag = Expression("1"),
                # State defaults to "Present"

                # 2. bioavail: Expression with multiple StParms, State = Searched
                bioavail = Expression(
                  "F1 * F2",
                  ContainedStParms = list(F1_stparm, F2_stparm),
                  State = "Searched"
                ),

                # 3. rate: Expression with math on an StParm
                rate = Expression("BaseRate / 2",
                                  ContainedStParms = list(BaseRate_stparm)) # State defaults to "Present"
              )

            # --- Verification
            modified_dosepoint <- modelPMLCodes$PK1FOC$MainDosepoint$Aa

            # Check tlag
            testthat::expect_s3_class(modified_dosepoint$tlag, "Expression")
            testthat::expect_equal(modified_dosepoint$tlag$ExpressionText, "1")
            testthat::expect_equal(length(modified_dosepoint$tlag$ContainedStParms), 0)

            # Check bioavail
            testthat::expect_s3_class(modified_dosepoint$bioavail, "Expression")
            testthat::expect_equal(modified_dosepoint$bioavail$ExpressionText, "F1 * F2")
            testthat::expect_equal(modified_dosepoint$bioavail$State, "Searched")
            testthat::expect_equal(length(modified_dosepoint$bioavail$ContainedStParms), 2)
            testthat::expect_equal(modified_dosepoint$bioavail$ContainedStParms[[1]]$StParmName,
                                   "F1")
            testthat::expect_equal(modified_dosepoint$bioavail$ContainedStParms[[2]]$StParmName,
                                   "F2")

            # Check rate
            testthat::expect_s3_class(modified_dosepoint$rate, "Expression")
            testthat::expect_equal(modified_dosepoint$rate$ExpressionText, "BaseRate / 2")
            testthat::expect_equal(modified_dosepoint$rate$State, "Present")
            testthat::expect_equal(length(modified_dosepoint$rate$ContainedStParms), 1)
            testthat::expect_equal(modified_dosepoint$rate$ContainedStParms[[1]]$StParmName,
                                   "BaseRate")

            # --- Snapshot Generation ---
            TemplateFilePath <-
              file.path(TestFolder, "template_expr_test.txt")
            TokensFilePath <- file.path(TestFolder, "tokens_expr_test.json")
            DataFilePath <- file.path(TestFolder, "Data_expr_test.csv")
            # Create dummy CSV content
            write.csv(
              data.frame(
                ID = "ID",
                time = "time",
                AaDose = "AaDose",
                CObs = "CObs",
                TlagColumn = "TlagColumn" # Column needed for tlag expression
              ),
              DataFilePath,
              row.names = FALSE
            )

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test Expression class in Dosepoint",
                Author = "Certara",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  ID = "ID",
                  time = "time",
                  Aa = "AaDose",
                  CObs = "CObs",
                  TlagColumn = "TlagColumn" # Map the column for tlag
                ),
                PMLParametersSets = modelPMLCodes,
                OmegaSearchBlocks = list() # Keep simple for this test focus
              )

            output$Template$DATA <- NULL
            testthat::expect_snapshot_output(cat(unlist(output$Template), sep = "\n"))
            testthat::expect_snapshot_value(output$TokensList, style = "json2")
          })
