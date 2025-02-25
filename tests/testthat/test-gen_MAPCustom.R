test_that("AMT is mapped correctly for custom models ",
          {
            TestFolder <- tempdir()

            modelPMLCodes <-
              create_ModelPK(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Absorption = c("Weibull")
              )

            CustomCode <-
              c("test() {",
            "\n\tcfMicro(A1, Cl / V, Cl2 / V, Cl2 / V2, first = (Aa = Ka))",
            "\n\tC = A1 / V\n\tdosepoint(Aa)",
            "\n\terror(CEps = 0.1)",
            "\n\tobserve(CObs = C * (1 + CEps))",
            "\n\tstparm(Cl = tvCl * exp( nCl ))",
            "\n\tfixef(tvCl= c(, 1, ))",
            "\n\tranef(diag(nCl) = c(1))",
            "\n\tstparm(V = tvV * exp( nV ))",
            "\n\tfixef(tvV= c(, 1, ))",
            "\n\tranef(diag(nV) = c(1))",
            "\n\tstparm(Cl2 = tvCl2 * exp( nCl2 ))",
            "\n\tfixef(tvCl2= c(, 1, ))",
            "\n\tranef(diag(nCl2) = c(1))",
            "\n\tstparm(V2 = tvV2 * exp( nV2 ))",
            "\n\tfixef(tvV2= c(, 1, ))",
            "\n\tranef(diag(nV2) = c(1))",
            "\n\tstparm(Ka = tvKa)",
            "\n\tfixef(tvKa= c(, 1, ))",
            "\n\tevent(Death, haz)",
            "\n\tfixef(haz = 10)\n}")

            modelPMLCodes <- add_CustomSpace(modelPMLCodes, CustomCode)
            names(modelPMLCodes)[2] <- "Custom1"

            TemplateFilePath <-
              file.path(TestFolder, "template.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokensCustom.json")
            DataFilePath <-
              file.path(TestFolder, "OneCpt_OralBolus.csv")

            write.csv(data.frame(
              ID = "ID",
              time = "time",
              Dose = "Dose",
              CObs = "CObs",
              Death = "Death"
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
                  CObs = "CObs",
                  Death = "Death"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = "",
                AppendixRows = ""
              )

            output$Template$DATA <- NULL
            testthat::expect_snapshot_output(cat(unlist(output$Template), sep = "\n"))
            testthat::expect_snapshot_value(output$TokensList, style = "json2")
          })
