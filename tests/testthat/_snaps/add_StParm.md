# add_StParm remove_StParm work correctly

    Code
      TestPml1
    Output
      PK1IVC 
       test() {
      	cfMicro(A1, Cl / V)
      	C = A1 / V
      	dosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)
      	error(CEps = 0.1)
      	observe(CObs = C * (1 + CEps))
      	
      	stparm(Cl = tvCl * exp( nCl ))
      	fixef(tvCl= c(, 1, ))
      	ranef(diag(nCl) = c(1))
      	stparm(V = tvV + nV)
      	fixef(tvV= c(, 1, ))
      	ranef(diag(nV) = c(1))
      
      } 
      PK2IVC 
       test() {
      	cfMicro(A1, Cl / V, Cl2 / V, Cl2 / V2)
      C = A1 / V
      	dosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)
      	error(CEps = 0.1)
      	observe(CObs = C * (1 + CEps))
      	
      	stparm(Cl = tvCl * exp( nCl ))
      	fixef(tvCl= c(, 1, ))
      	ranef(diag(nCl) = c(1))
      	stparm(V = tvV + nV)
      	fixef(tvV= c(, 1, ))
      	ranef(diag(nV) = c(1))
      	stparm(Cl2 = tvCl2 * exp( nCl2 ))
      	fixef(tvCl2= c(, 1, ))
      	ranef(diag(nCl2) = c(1))
      	stparm(V2 = tvV2 * exp( nV2 ))
      	fixef(tvV2= c(, 1, ))
      	ranef(diag(nV2) = c(1))
      
      } 

