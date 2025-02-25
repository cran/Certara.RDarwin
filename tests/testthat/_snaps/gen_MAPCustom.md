# AMT is mapped correctly for custom models 

    ##Description: SearchElimTypeTlag
    ##Author: Certara
    ##MAP {PML[1]}
    ##MODEL {PML[2]}
    ##ESTARGS
    ##TABLES
    

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["PML"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["PK1WC", "Custom1"]
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
              "value": ["  A1 = Dose CObs = CObs id = ID time = time", "test() {\n\tdelayInfCpt(A1, MeanDelayTime, ShapeParamMinusOne, out = - Cl * C, dist = Weibull)\n\tC = A1 / V\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\t\n\tstparm(MeanDelayTime = tvMeanDelayTime * exp( nMeanDelayTime ))\n\tfixef(tvMeanDelayTime= c(, 1, ))\n\tranef(diag(nMeanDelayTime) = c(1))\n\tstparm(ShapeParamMinusOne = tvShapeParamMinusOne * exp( nShapeParamMinusOne ))\n\tfixef(tvShapeParamMinusOne= c(, 1, ))\n\tranef(diag(nShapeParamMinusOne) = c(1))\n\tstparm(Cl = tvCl * exp( nCl ))\n\tfixef(tvCl= c(, 1, ))\n\tranef(diag(nCl) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\n}"]
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
              "value": ["  Aa = Dose CObs = CObs Death = Death id = ID time = time", "test() {\n\t \n\t\tcfMicro(A1, Cl / V, Cl2 / V, Cl2 / V2, first = (Aa = Ka)) \n\t\tC = A1 / V\n\t\tdosepoint(Aa) \n\t\terror(CEps = 0.1) \n\t\tobserve(CObs = C * (1 + CEps)) \n\t\tstparm(Cl = tvCl * exp( nCl )) \n\t\tfixef(tvCl= c(, 1, )) \n\t\tranef(diag(nCl) = c(1)) \n\t\tstparm(V = tvV * exp( nV )) \n\t\tfixef(tvV= c(, 1, )) \n\t\tranef(diag(nV) = c(1)) \n\t\tstparm(Cl2 = tvCl2 * exp( nCl2 )) \n\t\tfixef(tvCl2= c(, 1, )) \n\t\tranef(diag(nCl2) = c(1)) \n\t\tstparm(V2 = tvV2 * exp( nV2 )) \n\t\tfixef(tvV2= c(, 1, )) \n\t\tranef(diag(nV2) = c(1)) \n\t\tstparm(Ka = tvKa) \n\t\tfixef(tvKa= c(, 1, )) \n\t\tevent(Death, haz) \n\t\tfixef(haz = 10)\n\t\n\t\n\n}"]
            }
          ]
        }
      ]
    }

