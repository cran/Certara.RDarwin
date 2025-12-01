# Dosepoint: Set tlag(Searched), duration(StParm/Expression with StParm), Omega searched

    ##Description: Test1v4: Search Tlag, diff durations(Expr/StParm), search nV
    ##Author: Certara
    ##MAP   {PML[1]} ID = ID time = time
    ##MODEL {PML[2]}
    ##ESTARGS
     sort=FALSE
    ##TABLES
    

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["PML", "_Tlag", "_nV"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["PK1FOC", "PK1FOCE", "PK1FOCS", "PK1FOCSE"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" Aa = AaDose CObs = CObs", "test() {\n\tcfMicro(A1, Cl / V, first = (Aa = Ka))\n\tC = A1 / V\n\tdosepoint(Aa{_Tlag[1]}, duration = 1/Rate, idosevar = AaDose, infdosevar = AaInfDose, infratevar = AaInfRate)\n\t{_Tlag[2]}\n\tstparm(Rate = tvRate )\n\tfixef(tvRate= c(0, 1, ))\n\t\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\t\n\tstparm(Cl = tvCl * exp( nCl ))\n\tfixef(tvCl= c(, 1, ))\n\tranef(diag(nCl) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\tstparm(Ka = tvKa * exp( nKa ))\n\tfixef(tvKa= c(, 1, ))\n\tranef(diag(nKa) = c(1))\n\n\t#search_block(nKa, nV, nTlag)\n}"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" Aa = AaDose CObs = CObs", "test() {\n\tderiv(Aa = - Ka * Aa)\n\tderiv(A1 = Ka * Aa - Cl * C)\n\turinecpt(A0 = Cl * C)\n\tC = A1 / V\n\tdosepoint(Aa, idosevar = AaDose, infdosevar = AaInfDose, infratevar = AaInfRate)\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\terror(A0Eps = 0.1)\n\tobserve(A0Obs = A0 * (1 + A0Eps))\n\t\n\tstparm(Ka = tvKa * exp( nKa ))\n\tfixef(tvKa= c(, 1, ))\n\tranef(diag(nKa) = c(1))\n\tstparm(Cl = tvCl * exp( nCl ))\n\tfixef(tvCl= c(, 1, ))\n\tranef(diag(nCl) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\n\t#search_block(nKa, nV)\n}"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" Aa = AaDose CObs = CObs", "test() {\n\tderiv(Aa = - Ka * Aa)\n\tderiv(A1 = Ka * Aa - Vmax * C / (Km + C))\n\tC = A1 / V\n\tdosepoint(Aa, idosevar = AaDose, infdosevar = AaInfDose, infratevar = AaInfRate)\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\t\n\tstparm(Ka = tvKa * exp( nKa ))\n\tfixef(tvKa= c(, 1, ))\n\tranef(diag(nKa) = c(1))\n\tstparm(Vmax = tvVmax * exp( nVmax ))\n\tfixef(tvVmax= c(, 1, ))\n\tranef(diag(nVmax) = c(1))\n\tstparm(Km = tvKm * exp( nKm ))\n\tfixef(tvKm= c(, 1, ))\n\tranef(diag(nKm) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\n\t#search_block(nKa, nV)\n}"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" Aa = AaDose CObs = CObs", "test() {\n\tderiv(Aa = - Ka * Aa)\n\tderiv(A1 = Ka * Aa - Vmax * C / (Km + C))\n\turinecpt(A0 = Vmax * C / (Km + C))\n\tC = A1 / V\n\tdosepoint(Aa{_Tlag[1]}, duration = D, idosevar = AaDose, infdosevar = AaInfDose, infratevar = AaInfRate)\n\t{_Tlag[2]}\n\tstparm(D = tvD * exp( nD ))\n\tfixef(tvD= c(, 1, ))\n\tranef(diag(nD) = c(1))\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\terror(A0Eps = 0.1)\n\tobserve(A0Obs = A0 * (1 + A0Eps))\n\t\n\tstparm(Ka = tvKa * exp( nKa ))\n\tfixef(tvKa= c(, 1, ))\n\tranef(diag(nKa) = c(1))\n\tstparm(Vmax = tvVmax * exp( nVmax ))\n\tfixef(tvVmax= c(, 1, ))\n\tranef(diag(nVmax) = c(1))\n\tstparm(Km = tvKm * exp( nKm ))\n\tfixef(tvKm= c(, 1, ))\n\tranef(diag(nKm) = c(1))\n\tstparm(V = tvV {_nV[1]})\n\tfixef(tvV= c(, 1, ))\n\t{_nV[2]}\n\n\t#search_block(nKa, nV, nD, nTlag)\n}"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["StParm", "StParm"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["StParm", "StParm"]
                }
              },
              "value": [", tlag = Tlag", "stparm(Tlag = tvTlag * exp( nTlag ))\n\tfixef(tvTlag= c(, 1, ))\n\tranef(diag(nTlag) = c(1))"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Ranefs", "Ranefs"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Ranefs", "Ranefs"]
                }
              },
              "value": ["* exp( nV )", "ranef(diag(nV) = c(1))"]
            }
          ]
        }
      ]
    }

# Covariates searched on Cl and V: Sex

    ##Description: SearchCovariates
    ##Author: Certara
    ##MAP  Sex=Gender(female = 0, male = 1)  A1 = Amount CObs = Conc id = subject time = Act_Time Age = Age BW = BodyWeight
    ##MODEL test() {
    	cfMicro(A1, Cl / V, Cl2 / V, Cl2 / V2)
    C = A1 / V
    	dosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)
    	error(CEps = 0.1)
    	observe(CObs = C * (1 + CEps))
    	fcovariate(Sex())
    	stparm(Cl = tvCl {Cl_Sex[1]} * exp( nCl ))
    	fixef(tvCl= c(, 1, ))
    	{Cl_Sex[2]}
    	ranef(diag(nCl) = c(1))
    	stparm(V = tvV {V_Sex[1]} * exp( nV ))
    	fixef(tvV= c(, 1, ))
    	{V_Sex[2]}
    	ranef(diag(nV) = c(1))
    	stparm(Cl2 = tvCl2 * exp( nCl2 ))
    	fixef(tvCl2= c(, 1, ))
    	ranef(diag(nCl2) = c(1))
    	stparm(V2 = tvV2 * exp( nV2 ))
    	fixef(tvV2= c(, 1, ))
    	ranef(diag(nV2) = c(1))
    
    	#search_block(nCl2, nCl)
    	#search_block(nV2, nV)
    }
    ##ESTARGS
    ##TABLES
    

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["Cl_Sex", "V_Sex"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": [" * exp((Sex==1)*dCldSex1)", "\n\tfixef(dCldSex1= c(, 0, ))"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": [" * exp((Sex==1)*dVdSex1)", "\n\tfixef(dVdSex1= c(, 0, ))"]
            }
          ]
        }
      ]
    }

# Absorption types: Zero-Order, Gaussian, Inverse Gaussian, and Weibull 

    ##Description: SearchAbsorption
    ##Author: Certara
    ##MAP   {PML[1]} ID = ID time = time
    ##MODEL {PML[2]}
    ##ESTARGS
     sort=FALSE method=QRPEM
    ##SIMARGS
     numReplicates=200 seed=1 sort=FALSE
    ##TABLES
    

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["PML", "_D"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["PK1GCS", "PK1IVCS", "PK1IGCS", "PK1WCS"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" A1 = Dose CObs = CObs", "test() {\n\tdelayInfCpt(A1, MeanDelayTime, ShapeParamMinusOne, out = - Vmax * C / (Km + C), dist = Gamma)\n\tC = A1 / V\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\t\n\tstparm(MeanDelayTime = tvMeanDelayTime * exp( nMeanDelayTime ))\n\tfixef(tvMeanDelayTime= c(, 1, ))\n\tranef(diag(nMeanDelayTime) = c(1))\n\tstparm(ShapeParamMinusOne = tvShapeParamMinusOne * exp( nShapeParamMinusOne ))\n\tfixef(tvShapeParamMinusOne= c(, 1, ))\n\tranef(diag(nShapeParamMinusOne) = c(1))\n\tstparm(Vmax = tvVmax * exp( nVmax ))\n\tfixef(tvVmax= c(, 1, ))\n\tranef(diag(nVmax) = c(1))\n\tstparm(Km = tvKm * exp( nKm ))\n\tfixef(tvKm= c(, 1, ))\n\tranef(diag(nKm) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\n}"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" A1 = Dose CObs = CObs", "test() {\n\tderiv(A1 = - Vmax * C / (Km + C))\n\tC = A1 / V\n\tdosepoint(A1{_D[1]}, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\t{_D[2]}\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\t\n\tstparm(Vmax = tvVmax * exp( nVmax ))\n\tfixef(tvVmax= c(, 1, ))\n\tranef(diag(nVmax) = c(1))\n\tstparm(Km = tvKm * exp( nKm ))\n\tfixef(tvKm= c(, 1, ))\n\tranef(diag(nKm) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\n}"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" A1 = Dose CObs = CObs", "test() {\n\tdelayInfCpt(A1, MeanDelayTime, ShapeParam, out = - Vmax * C / (Km + C), dist = InverseGaussian)\n\tC = A1 / V\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\t\n\tstparm(MeanDelayTime = tvMeanDelayTime * exp( nMeanDelayTime ))\n\tfixef(tvMeanDelayTime= c(, 1, ))\n\tranef(diag(nMeanDelayTime) = c(1))\n\tstparm(ShapeParam = tvShapeParam * exp( nShapeParam ))\n\tfixef(tvShapeParam= c(, 1, ))\n\tranef(diag(nShapeParam) = c(1))\n\tstparm(Vmax = tvVmax * exp( nVmax ))\n\tfixef(tvVmax= c(, 1, ))\n\tranef(diag(nVmax) = c(1))\n\tstparm(Km = tvKm * exp( nKm ))\n\tfixef(tvKm= c(, 1, ))\n\tranef(diag(nKm) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\n}"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" A1 = Dose CObs = CObs", "test() {\n\tdelayInfCpt(A1, MeanDelayTime, ShapeParamMinusOne, out = - Vmax * C / (Km + C), dist = Weibull)\n\tC = A1 / V\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\t\n\tstparm(MeanDelayTime = tvMeanDelayTime * exp( nMeanDelayTime ))\n\tfixef(tvMeanDelayTime= c(, 1, ))\n\tranef(diag(nMeanDelayTime) = c(1))\n\tstparm(ShapeParamMinusOne = tvShapeParamMinusOne * exp( nShapeParamMinusOne ))\n\tfixef(tvShapeParamMinusOne= c(, 1, ))\n\tranef(diag(nShapeParamMinusOne) = c(1))\n\tstparm(Vmax = tvVmax * exp( nVmax ))\n\tfixef(tvVmax= c(, 1, ))\n\tranef(diag(nVmax) = c(1))\n\tstparm(Km = tvKm * exp( nKm ))\n\tfixef(tvKm= c(, 1, ))\n\tranef(diag(nKm) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\n}"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["StParm", "StParm"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["StParm", "StParm"]
                }
              },
              "value": [", duration = D", "stparm(D = tvD * exp( nD ))\n\tfixef(tvD= c(, 1, ))\n\tranef(diag(nD) = c(1))"]
            }
          ]
        }
      ]
    }

# RESE: AdditiveMultiplicative, MixRatio 

    ##Description: SearchRSE_enableRanefNewStparm
    ##Author: Certara
    ##MAP    A1 = Dose CObs = CObs ID = ID time = time
    ##MODEL test() {
    	cfMicro(A1, Cl / V, Cl2 / V, Cl2 / V2)
    C = A1 / V
    	dosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)
    	{_CObs[1]}
    	
    	stparm(Cl = tvCl * exp( nCl ))
    	fixef(tvCl= c(, 1, ))
    	ranef(diag(nCl) = c(1))
    	stparm(V = tvV * exp( nV ))
    	fixef(tvV= c(, 1, ))
    	ranef(diag(nV) = c(1))
    	stparm(Cl2 = tvCl2 * exp( nCl2 ))
    	fixef(tvCl2= c(, 1, ))
    	ranef(diag(nCl2) = c(1))
    	stparm(V2 = tvV2 * exp( nV2 ))
    	fixef(tvV2= c(, 1, ))
    	ranef(diag(nV2) = c(1))
    
    }
    ##ESTARGS
     sort=FALSE ODE=DVERK
    ##TABLES
    

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["_CObs"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Observation"]
                }
              },
              "value": ["error(CEps(freeze) = 0.01)\n\tobserve(CObs = C + CEps*sqrt(1 + C*C*(CMultStdev/sigma())^2))\n\tstparm(CMultStdev = tvCMultStdev )\n\tfixef(tvCMultStdev= c(, 0.1, ))\n\t"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Observation"]
                }
              },
              "value": ["error(CEps(freeze) = 0.01)\n\tobserve(CObs = C + CEps*(1 + C*CMixRatio))\n\tstparm(CMixRatio = tvCMixRatio )\n\tfixef(tvCMixRatio= c(, 10, ))\n\t"]
            }
          ]
        }
      ]
    }

# Covariates searched 

    ##Description: SearchCovariates
    ##Author: Certara
    ##MAP  Age=Age BW=BodyWeight Sex=Gender Occasion=Occasion  A1 = Dose CObs = Conc ID = ID time = time
    ##MODEL test() {
    	cfMicro(A1, Cl / V, Cl2 / V, Cl2 / V2)
    C = A1 / V
    	dosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)
    	error(CEps = 1)
    	observe(CObs = C + C^0.5*CEps, bql=22)
    	fcovariate(Age)
    	fcovariate(BW)
    	fcovariate(Sex())
    	fcovariate(Occasion())
    	stparm(Cl = tvCl {Cl_Age[1]} {Cl_BW[1]} {Cl_Sex[1]} * exp( nCl ))
    	fixef(tvCl= c(, 1, ))
    	{Cl_Age[2]}
    	{Cl_BW[2]}
    	{Cl_Sex[2]}
    	ranef(diag(nCl) = c(1))
    	stparm(V = tvV {_nV[1]})
    	fixef(tvV= c(, 1, ))
    	{_nV[2]}
    	stparm(Cl2 = tvCl2 * exp( nCl2 ))
    	fixef(tvCl2= c(, 1, ))
    	ranef(diag(nCl2) = c(1))
    	stparm(V2 = tvV2 * exp( nV2 ))
    	fixef(tvV2= c(, 1, ))
    	ranef(diag(nV2) = c(1))
    
    }
    ##ESTARGS
     sort=FALSE ODE=AutoDetect method=QRPEM numSampleSIR=15 numBurnIn=1 MCPEM=TRUE
    ##TABLES
    

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["Cl_Age", "Cl_BW", "Cl_Sex", "_nV"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": [" * (Age/median(Age))^dCldAge", "fixef(dCldAge= c(, 0, ))"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": [" * (BW/30)^dCldBW", "fixef(dCldBW= c(, 0, ))"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": [" * exp((Sex==1)*dCldSex1)", "\n\tfixef(dCldSex1= c(, 0, ))"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Ranefs", "Ranefs"]
                }
              },
              "value": ["* exp( nV )", "ranef(diag(nV) = c(1))"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Ranefs", "Ranefs"]
                }
              },
              "value": ["* exp( nV + (Occasion==1)*nVOccasionx1 + (Occasion==2)*nVOccasionx2 + (Occasion==3)*nVOccasionx3 )", "ranef(diag(nV) = c(1))\n\tranef(diag(nVOccasionx1) = c(1) , same(nVOccasionx2) , same(nVOccasionx3))"]
            }
          ]
        }
      ]
    }

---

    ##Description: Ellipsis
    ##Author: Certara
    ##MAP  Age=Age Weight=Weight {PML[1]} ID = ID time = time
    ##MODEL {PML[2]}
    ##ESTARGS
     sort=FALSE numIterations=30 method=FOCE-LB stdErr=Auto-Detect numIterNonParametric=3
    ##TABLES
    

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["PML", "_Tlag", "Tlag_Age", "Tlag_Weight", "CMultStdev_Weight", "_CObs", "_InEst_tvCl", "Cl_Weight", "V_Weight", "Ka_Weight", "Rate_Weight", "MeanDelayTime_Weight", "ShapeParamMinusOne_Weight"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["PK1FOC", "PK1WC"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" Aa = AMT CObs = Conc CObsBQL = CObsBQL", "test() {\n\tcfMicro(A1, Cl / V, first = (Aa = Ka))\n\tC = A1 / V\n\tdosepoint(Aa{_Tlag[1]}, idosevar = AaDose, infdosevar = AaInfDose, infratevar = AaInfRate)\n\t{_Tlag[2]}\n\t{_CObs[1]}\n\tfcovariate(Weight)\n\tcovariate(Age())\n\tstparm(Cl = exp( tvCl {Cl_Weight[1]} + nCl ))\n\tfixef(tvCl= c({_InEst_tvCl[1]}))\n\t{Cl_Weight[2]}\n\tranef(diag(nCl) = c(1))\n\tstparm(V = tvV {V_Weight[1]} * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\t{V_Weight[2]}\n\tranef(diag(nV) = c(0.1))\n\tstparm(Ka = tvKa {Ka_Weight[1]} * exp( nKa ))\n\tfixef(tvKa= c(, 10, ))\n\t{Ka_Weight[2]}\n\tranef(diag(nKa) = c(1))\n\n}"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" A1 = AMT CObs = Conc CObsBQL = CObsBQL", "test() {\n\tdelayInfCpt(A1, MeanDelayTime, ShapeParamMinusOne, out = - Cl * C, dist = Weibull)\n\tC = A1 / V\n\tdosepoint(A1, rate = Rate, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\tstparm(Rate = tvRate {Rate_Weight[1]} * exp( nRate ))\n\tfixef(tvRate= c(, 1, ))\n\t{Rate_Weight[2]}\n\tranef(diag(nRate) = c(1))\n\t{_CObs[1]}\n\tfcovariate(Weight)\n\tstparm(MeanDelayTime = tvMeanDelayTime {MeanDelayTime_Weight[1]} * exp( nMeanDelayTime ))\n\tfixef(tvMeanDelayTime= c(, 1, ))\n\t{MeanDelayTime_Weight[2]}\n\tranef(diag(nMeanDelayTime) = c(1))\n\tstparm(ShapeParamMinusOne = tvShapeParamMinusOne {ShapeParamMinusOne_Weight[1]} * exp( nShapeParamMinusOne ))\n\tfixef(tvShapeParamMinusOne= c(, 1, ))\n\t{ShapeParamMinusOne_Weight[2]}\n\tranef(diag(nShapeParamMinusOne) = c(1))\n\tstparm(Cl = exp( tvCl {Cl_Weight[1]} + nCl ))\n\tfixef(tvCl= c({_InEst_tvCl[1]}))\n\t{Cl_Weight[2]}\n\tranef(diag(nCl) = c(1))\n\tstparm(V = tvV {V_Weight[1]} * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\t{V_Weight[2]}\n\tranef(diag(nV) = c(0.1))\n\n}"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["StParm", "StParm"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["StParm", "StParm"]
                }
              },
              "value": [", tlag = Tlag", "stparm(Tlag = tvTlag {Tlag_Age[1]} {Tlag_Weight[1]} * exp( nTlag ))\n\tfixef(tvTlag= c(, 1, ))\n\t{Tlag_Age[2]}\n\t{Tlag_Weight[2]}\n\tranef(diag(nTlag) = c(1))"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": [" * exp((Age==2)*dTlagdAge2) * exp((Age==3)*dTlagdAge3)", "\n\tfixef(dTlagdAge2= c(, 0, ))\n\tfixef(dTlagdAge3= c(, 0, ))"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": [" * (Weight/median(Weight))^dTlagdWeight", "fixef(dTlagdWeight= c(, 0, ))"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": [" * (Weight/median(Weight))^dCMultStdevdWeight", "fixef(dCMultStdevdWeight= c(, 0, ))"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Observation"]
                }
              },
              "value": ["error(CEps = 1)\n\tobserve(CObs = C * (1 + CEps), bql)"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Observation"]
                }
              },
              "value": ["error(CEps = 2)\n\tobserve(CObs = C + CEps*sqrt(1 + C*C*(CMultStdev/sigma())^2), bql)\n\tstparm(CMultStdev = tvCMultStdev {CMultStdev_Weight[1]} )\n\tfixef(tvCMultStdev= c(, 0.1, ))\n\t{CMultStdev_Weight[2]}\n\t"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["ThetaInit"]
                }
              },
              "value": [", 0.2, "]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["ThetaInit"]
                }
              },
              "value": ["0, 3, 10"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": [" + (Weight-median(Weight))*dCldWeight", "fixef(dCldWeight= c(, 0, ))"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": [" * (Weight/median(Weight))^dVdWeight", "fixef(dVdWeight= c(, 0, ))"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": [" * (Weight/median(Weight))^dKadWeight", "fixef(dKadWeight= c(, 0, ))"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": [" * (Weight/median(Weight))^dRatedWeight", "fixef(dRatedWeight= c(, 0, ))"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": [" * (Weight/median(Weight))^dMeanDelayTimedWeight", "fixef(dMeanDelayTimedWeight= c(, 0, ))"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": ["", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CovThetaInStParm", "CovThetaInFixef"]
                }
              },
              "value": [" * (Weight/median(Weight))^dShapeParamMinusOnedWeight", "fixef(dShapeParamMinusOnedWeight= c(, 0, ))"]
            }
          ]
        }
      ]
    }

# StParms in Sigmas correctly named 

    ##Description: SearchNumCptElimTypeRSE
    ##Author: Certara
    ##MAP   {PML[1]} id = SubID time = time
    ##MODEL {PML[2]}
    ##ESTARGS
     sort=FALSE numIterations=10
    ##TABLES
    

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["PML", "_CObs", "_A0Obs"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["PK1IVCE", "PK1IVCSE", "PK2IVCE", "PK2IVCSE", "PK3IVCE", "PK3IVCSE"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" A1 = Dose CObs = CObs", "test() {\n\tderiv(A1 = -Cl * C)\n\turinecpt(A0 = Cl * C)\n\tC = A1 / V\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\t{_CObs[1]}\n\t{_A0Obs[1]}\n\t\n\tstparm(Cl = tvCl * exp( nCl ))\n\tfixef(tvCl= c(, 1, ))\n\tranef(diag(nCl) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\n}"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" A1 = Dose CObs = CObs", "test() {\n\tderiv(A1 = - Vmax * C / (Km + C))\n\turinecpt(A0 = Vmax * C / (Km + C))\n\tC = A1 / V\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\t{_CObs[1]}\n\t{_A0Obs[1]}\n\t\n\tstparm(Vmax = tvVmax * exp( nVmax ))\n\tfixef(tvVmax= c(, 1, ))\n\tranef(diag(nVmax) = c(1))\n\tstparm(Km = tvKm * exp( nKm ))\n\tfixef(tvKm= c(, 1, ))\n\tranef(diag(nKm) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\n}"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" A1 = Dose CObs = CObs", "test() {\n\tderiv(A1 = - Cl * C - Cl2 * (C - C2))\n\tderiv(A2 = Cl2 * (C - C2))\n\turinecpt(A0 = Cl * C)\n\tC = A1 / V\n\tC2 = A2 / V2\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\t{_CObs[1]}\n\t{_A0Obs[1]}\n\t\n\tstparm(Cl = tvCl * exp( nCl ))\n\tfixef(tvCl= c(, 1, ))\n\tranef(diag(nCl) = c(1))\n\tstparm(Cl2 = tvCl2 * exp( nCl2 ))\n\tfixef(tvCl2= c(, 1, ))\n\tranef(diag(nCl2) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\tstparm(V2 = tvV2 * exp( nV2 ))\n\tfixef(tvV2= c(, 1, ))\n\tranef(diag(nV2) = c(1))\n\n}"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" A1 = Dose CObs = CObs", "test() {\n\tderiv(A1 = - Vmax * C / (Km + C) - Cl2 * (C - C2))\n\tderiv(A2 = Cl2 * (C - C2))\n\turinecpt(A0 = Vmax * C / (Km + C))\n\tC = A1 / V\n\tC2 = A2 / V2\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\t{_CObs[1]}\n\t{_A0Obs[1]}\n\t\n\tstparm(Vmax = tvVmax * exp( nVmax ))\n\tfixef(tvVmax= c(, 1, ))\n\tranef(diag(nVmax) = c(1))\n\tstparm(Km = tvKm * exp( nKm ))\n\tfixef(tvKm= c(, 1, ))\n\tranef(diag(nKm) = c(1))\n\tstparm(Cl2 = tvCl2 * exp( nCl2 ))\n\tfixef(tvCl2= c(, 1, ))\n\tranef(diag(nCl2) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\tstparm(V2 = tvV2 * exp( nV2 ))\n\tfixef(tvV2= c(, 1, ))\n\tranef(diag(nV2) = c(1))\n\n}"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" A1 = Dose CObs = CObs", "test() {\n\tderiv(A1 = - Cl * C - Cl2 * (C - C2) - Cl3 * (C - C3))\n\tderiv(A2 = Cl2 * (C - C2))\n\tderiv(A3 = Cl3 * (C - C3))\n\turinecpt(A0 = Cl * C)\n\tC = A1 / V\n\tC2 = A2 / V2\n\tC3 = A3 / V3\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\t{_CObs[1]}\n\t{_A0Obs[1]}\n\t\n\tstparm(Cl = tvCl * exp( nCl ))\n\tfixef(tvCl= c(, 1, ))\n\tranef(diag(nCl) = c(1))\n\tstparm(Cl2 = tvCl2 * exp( nCl2 ))\n\tfixef(tvCl2= c(, 1, ))\n\tranef(diag(nCl2) = c(1))\n\tstparm(Cl3 = tvCl3 * exp( nCl3 ))\n\tfixef(tvCl3= c(, 1, ))\n\tranef(diag(nCl3) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\tstparm(V2 = tvV2 * exp( nV2 ))\n\tfixef(tvV2= c(, 1, ))\n\tranef(diag(nV2) = c(1))\n\tstparm(V3 = tvV3 * exp( nV3 ))\n\tfixef(tvV3= c(, 1, ))\n\tranef(diag(nV3) = c(1))\n\n}"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["MAPText", "PMLText"]
                }
              },
              "value": [" A1 = Dose CObs = CObs", "test() {\n\tderiv(A1 = - Vmax * C / (Km + C) - Cl2 * (C - C2) - Cl3 * (C - C3))\n\tderiv(A2 = Cl2 * (C - C2))\n\tderiv(A3 = Cl3 * (C - C3))\n\turinecpt(A0 = Vmax * C / (Km + C))\n\tC = A1 / V\n\tC2 = A2 / V2\n\tC3 = A3 / V3\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\t{_CObs[1]}\n\t{_A0Obs[1]}\n\t\n\tstparm(Vmax = tvVmax * exp( nVmax ))\n\tfixef(tvVmax= c(, 1, ))\n\tranef(diag(nVmax) = c(1))\n\tstparm(Km = tvKm * exp( nKm ))\n\tfixef(tvKm= c(, 1, ))\n\tranef(diag(nKm) = c(1))\n\tstparm(Cl2 = tvCl2 * exp( nCl2 ))\n\tfixef(tvCl2= c(, 1, ))\n\tranef(diag(nCl2) = c(1))\n\tstparm(Cl3 = tvCl3 * exp( nCl3 ))\n\tfixef(tvCl3= c(, 1, ))\n\tranef(diag(nCl3) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\tstparm(V2 = tvV2 * exp( nV2 ))\n\tfixef(tvV2= c(, 1, ))\n\tranef(diag(nV2) = c(1))\n\tstparm(V3 = tvV3 * exp( nV3 ))\n\tfixef(tvV3= c(, 1, ))\n\tranef(diag(nV3) = c(1))\n\n}"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Observation"]
                }
              },
              "value": ["error(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Observation"]
                }
              },
              "value": ["error(CEps = 0.01)\n\tobserve(CObs = C + CEps*sqrt(1 + C*C*(CMultStdev/sigma())^2))\n\tstparm(CMultStdev = tvCMultStdev )\n\tfixef(tvCMultStdev= c(, 0.1, ))\n\t"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Observation"]
                }
              },
              "value": ["error(A0Eps = 0.1)\n\tobserve(A0Obs = A0 * (1 + A0Eps), doafter={A0=0; })"]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Observation"]
                }
              },
              "value": ["error(A0Eps = 0.02)\n\tobserve(A0Obs = A0 + A0Eps*sqrt(1 + A0*A0*(A0MultStdev/sigma())^2), doafter={A0=0; })\n\tstparm(A0MultStdev = tvA0MultStdev )\n\tfixef(tvA0MultStdev= c(, 0.2, ))\n\t"]
            }
          ]
        }
      ]
    }

# Dosepoint: Expression class usage (text, multi-StParm, math, states)

    ##Description: Test Expression class in Dosepoint
    ##Author: Certara
    ##MAP    Aa = AaDose CObs = CObs ID = ID time = time TlagColumn = TlagColumn
    ##MODEL test() {
    	cfMicro(A1, Cl / V, first = (Aa = Ka))
    	C = A1 / V
    	dosepoint(Aa, tlag = 1{Aa_bioavail_Expression[1]}, rate = BaseRate / 2, idosevar = AaDose, infdosevar = AaInfDose, infratevar = AaInfRate)
    	{Aa_bioavail_Expression[2]}
    	{Aa_bioavail_Expression[3]}
    	stparm(BaseRate = tvBaseRate * exp( nBaseRate ))
    	fixef(tvBaseRate= c(, 5, ))
    	ranef(diag(nBaseRate) = c(1))
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
    ##ESTARGS
     sort=FALSE
    ##TABLES
    

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["Aa_bioavail_Expression"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Expression", "Expression", "Expression"]
                }
              },
              "value": ["", "", ""]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Expression", "Expression", "Expression"]
                }
              },
              "value": [", bioavail = F1 * F2", "stparm(F1 = tvF1 * exp( nF1 ))\n\tfixef(tvF1= c(, 0.8, ))\n\tranef(diag(nF1) = c(1))", "stparm(F2 = tvF2 * exp( nF2 ))\n\tfixef(tvF2= c(, 0.9, ))\n\tranef(diag(nF2) = c(1))"]
            }
          ]
        }
      ]
    }

