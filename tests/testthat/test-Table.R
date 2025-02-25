test_that("Tables are generated: ", {
  PMLParametersSets <-
    get_PMLParametersSets(CompartmentsNumber = c(1,2),
                          Absorption = c("First-Order", "Gamma"),
                          WT = Covariate(Name = "WT",
                                         StParmName = "V"))
  # write test data frame
  TempFolder <- tempdir()
  TemplateFilePath <- file.path(TempFolder, "template.txt")
  TokensFilePath <- file.path(TempFolder, "tokens.json")
  DataFilePath <- file.path(TempFolder, "Data.csv")
  write.csv(data.frame(id = 'id',
                       time = 'time',
                       AMT = 'AMT',
                       Conc = 'Conc',
                       age = 'age',
                       Weight = 'Weight',
                       CObsBQL = 'CObsBQL'),
            DataFilePath)



  output <-
    write_ModelTemplateTokens(TemplateFilePath = TemplateFilePath,
                            TokensFilePath = TokensFilePath,
                            Description = "1-2Cpts try",
                            Author = "Certara",
                            DataFilePath = DataFilePath,
                            DataMapping = c(ID = "id",
                                            time = "time",
                                            CObs = "Conc",
                                            AMT = "AMT",
                                            WT = "Weight"),
                            ColDef = "",
                            PMLParametersSets = PMLParametersSets,
                            EstArgs = specify_EngineParams(),
                            SimArgs = specify_SimParams(numReplicates = 200),
                            Tables = list(Table(Name = "simtable1.csv",
                                                TimesList = "seq(1,4,1)",
                                                WhenDose = "A1",
                                                VariablesList = "C",
                                                ForSimulation = TRUE),
                                          Table(Name = "table1.csv",
                                                WhenDose = c("A1", "Aa"),
                                                CovrSet = "WT")))

  output$Template$DATA <- NULL
  testthat::expect_snapshot_output(cat(unlist(output$Template), sep = "\n"))
  testthat::expect_snapshot_value(output$TokensList, style = "json2")
})
