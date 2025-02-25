test_that("modify_StParmCustom works correctly ",
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
            modelPMLCodes <-
              modify_StParmCustom(modelPMLCodes, "Cl", Type = "Normal")

            testthat::expect_snapshot_value(modelPMLCodes$l425$PMLCode)

            CustomCode <- "deriv(Aa1 = -Ktr * Aa1)
  deriv(Aa2 = Ktr * (Aa1 - Aa2))
  deriv(A1 = Ktr * Aa2 - Cl * C)
	dosepoint(Aa1)
	C = A1 / V
	error(CEps = 0.1)
	observe(CObs = C * (1 + CEps))
	fcovariate(OCC())
	stparm(V = tvV * exp(nV + nVx0*(OCC==1) + nVx1*(OCC==2) + nVx2*(OCC==3)))
	stparm(Cl = tvCl * exp(nCl))
	stparm(Ktr = tvKtr * exp(nKtr))
	fixef(tvV = c(, 5, ))
	fixef(tvCl = c(, 1, ))
	fixef(tvKtr = c(, 1, ))
	ranef(diag(nV) = c(1))
	ranef(diag(nVx0) = c(1), same(nVx1), same(nVx2))
	ranef(diag(nCl) = c(1))
	ranef(diag(nKtr) = c(1))"

            ## Create a simple built-in model and then add a custom model to it
            models <-
              create_ModelPK(Absorption = c("First-Order"))
            models <-
              add_CustomSpace(models, CustomCode = CustomCode)

            ## modify V
            updatedModels <-
              modify_StParmCustom(models, StParmName = "V", Type = "LogNormal2")

            testthat::expect_snapshot_value(updatedModels, style = "json2")
          })
