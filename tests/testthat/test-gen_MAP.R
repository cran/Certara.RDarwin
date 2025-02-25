test_that("AMT is mapped correctly ",
          {
            TestFolder <- tempdir()

            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Absorption = c("First-Order", "Gamma", "Inverse Gaussian", "Weibull")
              )

            TemplateFilePath <-
              file.path(TestFolder, "template.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokensTlagDuration.json")
            DataFilePath <-
              file.path(TestFolder, "OneCpt_OralBolus.csv")

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
                Description = "SearchElimTypeTlag",
                Author = "Certara",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "ID",
                  time = "time",
                  AMT = "Dose",
                  CObs = "CObs"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = "",
                AppendixRows = ""
              )

            output$Template$DATA <- NULL
            testthat::expect_snapshot_output(cat(unlist(output$Template), sep = "\n"))
            testthat::expect_snapshot_value(output$TokensList, style = "json2")
          })
