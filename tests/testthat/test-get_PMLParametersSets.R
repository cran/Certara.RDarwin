test_that("Setting tlag, add duration, Omega searched",
          {
            TestFolder <- tempdir()

            modelPMLCodes <- get_PMLParametersSets(
              CompartmentsNumber = 1,
              Parameterization = "Clearance",
              Saturation = c(TRUE, FALSE),
              Absorption = "First-Order",
              EliminationCpt = c(TRUE, FALSE)
            )

            stparm_Tlag <- StParm("Tlag", State = "Searched")

            modelPMLCodes$PK1FOC$MainDosepoint$Aa$tlag <-
              stparm_Tlag
            modelPMLCodes$PK1FOCSE$MainDosepoint$Aa$tlag <-
              stparm_Tlag
            modelPMLCodes$PK1FOCSE$Observations$CObs$ResetObs <-
              FALSE
            modelPMLCodes$PK1FOCSE$StParms$V$OmegaStParm$State <-
              "Searched"
            modelPMLCodes$PK1FOCSE$MainDosepoint$Aa$duration <-
              StParm("D")
            modelPMLCodes$PK1FOC$MainDosepoint$Aa$duration <-
              "DColumn"

            TemplateFilePath <-
              file.path(TestFolder, "template.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokensTlagDuration.json")
            DataFilePath <-
              file.path(TestFolder, "OneCpt_OralBolus.csv")
            write.csv(
              data.frame(
                ID = "ID",
                time = "time",
                Dose = "Dose",
                CObs = "CObs",
                DColumn = "DColumn"
              ),
              DataFilePath
            )

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "SearchElimTypeTlag",
                Author = "Certara",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "ID",
                  time = "time",
                  Aa = "Dose",
                  CObs = "CObs",
                  DColumn = "DColumn"
                ),
                PMLParametersSets = modelPMLCodes,
                OmegaSearchBlocks = list(c("nKa", "nV", "nD"))
              )

            output$Template$DATA <- NULL
            testthat::expect_snapshot_output(cat(unlist(output$Template), sep = "\n"))
            testthat::expect_snapshot_value(output$TokensList, style = "json2")
          })

test_that("Covariates searched on Cl and V: Sex",
          {
            TestFolder <- tempdir()

            modelPMLCodes <-
              get_PMLParametersSets(CompartmentsNumber = 2,
                                    Parameterization = "Clearance")

            CatCov_Sex_Cl <- Covariate(
              Name = "Sex",
              StParmName = "Cl",
              Type = "Categorical",
              State = "Searched",
              Categories = c(0, 1)
            )

            CatCov_Sex_V <- Covariate(
              Name = "Sex",
              StParmName = "V",
              Type = "Categorical",
              State = "Searched",
              Categories = c(0, 1)
            )

            modelPMLCodes$PK2IVC$StParms$Cl$Covariates <-
              c(Sex = list(CatCov_Sex_Cl))
            modelPMLCodes$PK2IVC$StParms$V$Covariates <-
              c(Sex = list(CatCov_Sex_V))

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
            modelPMLCodes$PK1IVCS$MainDosepoint$A1$duration <-
              StParm("D", State = "Searched")

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
                  tDOF = 3L,
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
