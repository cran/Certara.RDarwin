test_that("dosepoint2 is parsed correctly ",
          {
            OneCpt_CustomCode <-
              paste0(
                "\n\n  deriv(A1 = - Cl * C)",
                "\n\tdosepoint(A1)",
                "\n\tdosepoint2(A1, tlag = 12)",
                "\n\tC = A1 / V",
                "## Residual error model",
                "\n\terror(CEps = 0.01)",
                "\n\tobserve(CObs = C + CEps * sqrt(1 + C^2 * (CMultStdev/sigma())^2), bql = 0.01)",
                "\n\tstparm(V = tvV * exp(nV))",
                "\n\tstparm(Cl = tvCl * exp(nCl))",
                "\n\tstparm(CMultStdev = tvCMultStdev)",
                "\n\tfixef(tvV = c(, 5, ))",
                "\n\tfixef(tvCl = c(, 1, ))",
                "\n\tfixef(tvCMultStdev = c(, 0.1, ))",
                "\n\n\tranef(diag(nV, nCl) = c(1, 1))\n\n"
              )

            modelPMLCodes <- create_CustomSpace(OneCpt_CustomCode)
            testthat::expect_snapshot_value(list_Dosepoints(modelPMLCodes), style = "json2")
            testthat::expect_snapshot_value(list_Observations(modelPMLCodes), style = "json2")
            testthat::expect_snapshot_value(list_Covariates(modelPMLCodes), style = "json2")
            testthat::expect_snapshot_value(list_Omegas(modelPMLCodes), style = "json2")
            testthat::expect_snapshot_value(list_StParms(modelPMLCodes), style = "json2")
            testthat::expect_snapshot_value(list_Thetas(modelPMLCodes), style = "json2")

            names(modelPMLCodes) <- "Custom1"
            modelPMLCodes$Custom1$CustomDosepoints[[1]]$PMLStructure <-
              "Custom1"
            modelPMLCodes$Custom1$CustomDosepoints[[2]]$PMLStructure <-
              "Custom1"

            testthat::expect_snapshot_value(modelPMLCodes$Custom1$CustomDosepoints, style = "json2")
          })

test_that("ranef is parsed correctly ",
          {
            TwoCpt_IOV_CustomCode <-
              paste0(
                "  cfMicro(A1, Cl / V, Cl2 / V, Cl2 / V2)",
                "	dosepoint(A1)",
                "	C = A1 / V",
                "",
                "	##===============================================================================",
                "	## Residual error model",
                "	##===============================================================================",
                "	error(CEps = 0.1) # true value: 0.16",
                "	observe(CObs = C * (1 + CEps))",
                "",
                "	##===============================================================================",
                "	## Model parameters",
                "	##===============================================================================",
                "	fcovariate(OCC())",
                "",
                "	## Structural parameters",
                "	stparm(V = tvV * exp(nV + nVx0*(OCC==1) + nVx1*(OCC==2) + nVx2*(OCC==3)))",
                "	stparm(Cl = tvCl * exp(nCl + nClx0*(OCC==1) + nClx1*(OCC==2) + nClx2*(OCC==3)))",
                "	stparm(V2 = tvV2 * exp(nV2))",
                "	stparm(Cl2 = tvCl2 * exp(nCl2))",
                "",
                "",
                "	## Fixed effects",
                "	fixef(tvV = c(, 5, )) # true value: 4.7",
                "	fixef(tvCl = c(, 2, )) # true value: 1.8",
                "	fixef(tvV2 = c(, 5, )) # true value: 3.5",
                "	fixef(tvCl2 = c(, 1, )) # true value: 0.7",
                "",
                "	## Random effects",
                "	ranef(diag(nV, nCl, nV2, nCl2) = c(1, 1, 1, 1)) # true value: c(0.25, 0.25, 0.36, 0.36)",
                "	ranef(diag(nVx0,nClx0) = c(1, 1), same(nVx1,nClx1), same(nVx2,nClx2))"
              )

            modelPMLCodes <-
              create_CustomSpace(TwoCpt_IOV_CustomCode)
            names(modelPMLCodes) <- "Custom1"

            testthat::expect_snapshot_value(modelPMLCodes$Custom1$CustomRanefs, style = "json2")
          })
