# Modify Omega in PMLParametersSets

    Code
      modify_Omega(PMLParametersSets, Name = "nV", InitialOmega = 0.5, Frozen = TRUE,
        State = "Present", PMLStructures = "PK1FOCS")
    Output
      PK1FOC 
       test() {
      	cfMicro(A1, Cl / V, first = (Aa = Ka))
      	C = A1 / V
      	dosepoint(Aa, idosevar = AaDose, infdosevar = AaInfDose, infratevar = AaInfRate)
      	error(CEps = 0.1)
      	observe(CObs = C * (1 + CEps))
      	
      	stparm(Cl = tvCl * exp( nCl ))
      	fixef(tvCl= c(, 1, ))
      	ranef(diag(nCl) = c(1))
      	stparm(V = tvV * exp( nV ))
      	fixef(tvV= c(, 1, ))
      	ranef(diag(nV) = c(1))
      	stparm(Ka = tvKa * exp( nKa ))
      	fixef(tvKa= c(, 1, ))
      	ranef(diag(nKa) = c(1))
      
      } 
      PK1FOCS 
       test() {
      	deriv(Aa = - Ka * Aa)
      	deriv(A1 = Ka * Aa - Vmax * C / (Km + C))
      	C = A1 / V
      	dosepoint(Aa, idosevar = AaDose, infdosevar = AaInfDose, infratevar = AaInfRate)
      	error(CEps = 0.1)
      	observe(CObs = C * (1 + CEps))
      	
      	stparm(Ka = tvKa * exp( nKa ))
      	fixef(tvKa= c(, 1, ))
      	ranef(diag(nKa) = c(1))
      	stparm(Vmax = tvVmax * exp( nVmax ))
      	fixef(tvVmax= c(, 1, ))
      	ranef(diag(nVmax) = c(1))
      	stparm(Km = tvKm * exp( nKm ))
      	fixef(tvKm= c(, 1, ))
      	ranef(diag(nKm) = c(1))
      	stparm(V = tvV * exp( nV ))
      	fixef(tvV= c(, 1, ))
      	ranef(diag(nV) (freeze) = c(0.5))
      
      } 

