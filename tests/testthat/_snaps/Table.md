# Tables are generated: 

    ##Description: 1-2Cpts try
    ##Author: Certara
    ##MAP  WT=Weight {PML[1]} ID = id time = time
    ##MODEL {PML[2]}
    ##ESTARGS
     sort=FALSE
    ##SIMARGS
     numReplicates=200 seed=1234 sort=FALSE
    ##TABLES
    simtbl(file="simtable1.csv", time(seq(1,4,1)), dose(A1), C)
    table(file="table1.csv", covr(WT), dose(A1, Aa))
    

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
              "value": ["PK1FOC", "PK1GC", "PK2FOC", "PK2GC"]
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
              "value": [" Aa = AMT CObs = Conc", "test() {\n\tcfMicro(A1, Cl / V, first = (Aa = Ka))\n\tC = A1 / V\n\tdosepoint(Aa, idosevar = AaDose, infdosevar = AaInfDose, infratevar = AaInfRate)\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\tfcovariate(WT)\n\tstparm(Cl = tvCl * exp( nCl ))\n\tfixef(tvCl= c(, 1, ))\n\tranef(diag(nCl) = c(1))\n\tstparm(V = tvV  * WT^dVdWT * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tfixef(dVdWT= c(, 0, ))\n\tranef(diag(nV) = c(1))\n\tstparm(Ka = tvKa * exp( nKa ))\n\tfixef(tvKa= c(, 1, ))\n\tranef(diag(nKa) = c(1))\n\n}"]
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
              "value": [" A1 = AMT CObs = Conc", "test() {\n\tdelayInfCpt(A1, MeanDelayTime, ShapeParamMinusOne, out = - Cl * C, dist = Gamma)\n\tC = A1 / V\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\tfcovariate(WT)\n\tstparm(MeanDelayTime = tvMeanDelayTime * exp( nMeanDelayTime ))\n\tfixef(tvMeanDelayTime= c(, 1, ))\n\tranef(diag(nMeanDelayTime) = c(1))\n\tstparm(ShapeParamMinusOne = tvShapeParamMinusOne * exp( nShapeParamMinusOne ))\n\tfixef(tvShapeParamMinusOne= c(, 1, ))\n\tranef(diag(nShapeParamMinusOne) = c(1))\n\tstparm(Cl = tvCl * exp( nCl ))\n\tfixef(tvCl= c(, 1, ))\n\tranef(diag(nCl) = c(1))\n\tstparm(V = tvV  * WT^dVdWT * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tfixef(dVdWT= c(, 0, ))\n\tranef(diag(nV) = c(1))\n\n}"]
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
              "value": [" Aa = AMT CObs = Conc", "test() {\n\tcfMicro(A1, Cl / V, Cl2 / V, Cl2 / V2, first = (Aa = Ka))\nC = A1 / V\n\tdosepoint(Aa, idosevar = AaDose, infdosevar = AaInfDose, infratevar = AaInfRate)\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\tfcovariate(WT)\n\tstparm(Cl = tvCl * exp( nCl ))\n\tfixef(tvCl= c(, 1, ))\n\tranef(diag(nCl) = c(1))\n\tstparm(V = tvV  * WT^dVdWT * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tfixef(dVdWT= c(, 0, ))\n\tranef(diag(nV) = c(1))\n\tstparm(Cl2 = tvCl2 * exp( nCl2 ))\n\tfixef(tvCl2= c(, 1, ))\n\tranef(diag(nCl2) = c(1))\n\tstparm(V2 = tvV2 * exp( nV2 ))\n\tfixef(tvV2= c(, 1, ))\n\tranef(diag(nV2) = c(1))\n\tstparm(Ka = tvKa * exp( nKa ))\n\tfixef(tvKa= c(, 1, ))\n\tranef(diag(nKa) = c(1))\n\n}"]
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
              "value": [" A1 = AMT CObs = Conc", "test() {\n\tdelayInfCpt(A1, MeanDelayTime, ShapeParamMinusOne, out = - Cl * C - Cl2 * (C - C2), dist = Gamma)\nderiv(A2 = Cl2 * (C - C2))\n\tC = A1 / V\n\tC2 = A2 / V2\n\tdosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)\n\terror(CEps = 0.1)\n\tobserve(CObs = C * (1 + CEps))\n\tfcovariate(WT)\n\tstparm(MeanDelayTime = tvMeanDelayTime * exp( nMeanDelayTime ))\n\tfixef(tvMeanDelayTime= c(, 1, ))\n\tranef(diag(nMeanDelayTime) = c(1))\n\tstparm(ShapeParamMinusOne = tvShapeParamMinusOne * exp( nShapeParamMinusOne ))\n\tfixef(tvShapeParamMinusOne= c(, 1, ))\n\tranef(diag(nShapeParamMinusOne) = c(1))\n\tstparm(Cl = tvCl * exp( nCl ))\n\tfixef(tvCl= c(, 1, ))\n\tranef(diag(nCl) = c(1))\n\tstparm(Cl2 = tvCl2 * exp( nCl2 ))\n\tfixef(tvCl2= c(, 1, ))\n\tranef(diag(nCl2) = c(1))\n\tstparm(V = tvV  * WT^dVdWT * exp( nV ))\n\tfixef(tvV= c(, 1, ))\n\tfixef(dVdWT= c(, 0, ))\n\tranef(diag(nV) = c(1))\n\tstparm(V2 = tvV2 * exp( nV2 ))\n\tfixef(tvV2= c(, 1, ))\n\tranef(diag(nV2) = c(1))\n\n}"]
            }
          ]
        }
      ]
    }

