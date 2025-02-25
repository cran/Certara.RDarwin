# AMT is mapped correctly 

    ##Description: SearchElimTypeTlag
    ##Author: Certara
    ##MAP   {PML[1]} id = ID time = time
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
              "value": ["PK1FOC", "PK1GC", "PK1IGC", "PK1WC"]
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
              "value": [" Aa = Dose CObs = CObs", "test() {\n\tcfMicro(A1, Cl / V, first = (Aa = Ka))\n\tC = A1 / V\n\tdosepoint(Aa, idosevar = AaDose, infdosevar = AaInfDose, infratevar = AaInfRate)\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\t\n\tstparm(Cl = tvCl * exp( nCl ))\n\tfixef(tvCl= c(, 1, ))\n\tranef(diag(nCl) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\tstparm(Ka = tvKa * exp( nKa ))\n\tfixef(tvKa= c(, 1, ))\n\tranef(diag(nKa) = c(1))\n\n}"]
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
              "value": [" A1 = Dose CObs = CObs", "test() {\n\tdelayInfCpt(A1, MeanDelayTime, ShapeParamMinusOne, out = - Cl * C, dist = Gamma)\n\tC = A1 / V\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\t\n\tstparm(MeanDelayTime = tvMeanDelayTime * exp( nMeanDelayTime ))\n\tfixef(tvMeanDelayTime= c(, 1, ))\n\tranef(diag(nMeanDelayTime) = c(1))\n\tstparm(ShapeParamMinusOne = tvShapeParamMinusOne * exp( nShapeParamMinusOne ))\n\tfixef(tvShapeParamMinusOne= c(, 1, ))\n\tranef(diag(nShapeParamMinusOne) = c(1))\n\tstparm(Cl = tvCl * exp( nCl ))\n\tfixef(tvCl= c(, 1, ))\n\tranef(diag(nCl) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\n}"]
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
              "value": [" A1 = Dose CObs = CObs", "test() {\n\tdelayInfCpt(A1, MeanDelayTime, ShapeParam, out = - Cl * C, dist = InverseGaussian)\n\tC = A1 / V\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\t\n\tstparm(MeanDelayTime = tvMeanDelayTime * exp( nMeanDelayTime ))\n\tfixef(tvMeanDelayTime= c(, 1, ))\n\tranef(diag(nMeanDelayTime) = c(1))\n\tstparm(ShapeParam = tvShapeParam * exp( nShapeParam ))\n\tfixef(tvShapeParam= c(, 1, ))\n\tranef(diag(nShapeParam) = c(1))\n\tstparm(Cl = tvCl * exp( nCl ))\n\tfixef(tvCl= c(, 1, ))\n\tranef(diag(nCl) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\n}"]
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
              "value": [" A1 = Dose CObs = CObs", "test() {\n\tdelayInfCpt(A1, MeanDelayTime, ShapeParamMinusOne, out = - Cl * C, dist = Weibull)\n\tC = A1 / V\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\t\n\tstparm(MeanDelayTime = tvMeanDelayTime * exp( nMeanDelayTime ))\n\tfixef(tvMeanDelayTime= c(, 1, ))\n\tranef(diag(nMeanDelayTime) = c(1))\n\tstparm(ShapeParamMinusOne = tvShapeParamMinusOne * exp( nShapeParamMinusOne ))\n\tfixef(tvShapeParamMinusOne= c(, 1, ))\n\tranef(diag(nShapeParamMinusOne) = c(1))\n\tstparm(Cl = tvCl * exp( nCl ))\n\tfixef(tvCl= c(, 1, ))\n\tranef(diag(nCl) = c(1))\n\tstparm(V = tvV * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tranef(diag(nV) = c(1))\n\n}"]
            }
          ]
        }
      ]
    }

