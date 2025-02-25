# Compare output StParm with implicit and explicit thetas for searched occasion covariate

    Code
      ClOmegasExplicit
    Output
      stparm(Cl = tvCl * exp( nCl {Cl_Period[1]} ))
      	fixef(tvCl= c(, 1, ))
      	ranef(diag(nCl) = c(1))
      	{Cl_Period[2]} 

---

    Code
      ClOmegasImplicit
    Output
      stparm(Cl = tvCl * exp( nCl {Cl_Period[1]} ))
      	fixef(tvCl= c(, 1, ))
      	ranef(diag(nCl) = c(1))
      	{Cl_Period[2]} 

