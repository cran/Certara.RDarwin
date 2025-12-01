# Dosepoint creates a valid Dosepoint object

    Code
      DosepointInstance
    Output
      dosepoint(A1, tlag = Tlag, bioavail = F, duration = D, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)
      	stparm(Tlag = tvTlag * exp( nTlag ))
      	fixef(tvTlag= c(, 1, ))
      	ranef(diag(nTlag) = c(1))
      	stparm(F = tvF * exp( nF ))
      	fixef(tvF= c(, 1, ))
      	ranef(diag(nF) = c(1))
      	stparm(D = tvD * exp( nD ))
      	fixef(tvD= c(, 1, ))
      	ranef(diag(nD) = c(1)) 

