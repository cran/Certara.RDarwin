# modify_StParmCustom works correctly 

    "\n\t\n\t  deriv(A1 = - Cl * C)\n\t\tdosepoint(A1)\n\t\tdosepoint2(A1, tlag = 12)\n\t\tC = A1 / V## Residual error model\n\t\terror(CEps = 0.01)\n\t\tobserve(CObs = C + CEps * sqrt(1 + C^2 * (CMultStdev/sigma())^2), bql = 0.01)\n\t\tstparm(V = tvV * exp(nV))\n\t\t\n\t\tstparm(CMultStdev = tvCMultStdev)\n\t\tfixef(tvV = c(, 5, ))\n\t\t\n\t\tfixef(tvCMultStdev = c(, 0.1, ))\n\t\n\t\tranef(diag(nV, nCl) = c(1, 1))\n\t\n\t"

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["PK1FOC", "l520"]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["PMLModels"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["Type", "CompartmentsNumber", "Absorption", "Parameterization", "Saturation", "EliminationCpt", "ClosedForm", "FractionExcreted", "PMLCode", "StParms", "Observations", "MainDosepoint"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["PK"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["First-Order"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Clearance"]
            },
            {
              "type": "logical",
              "attributes": {},
              "value": [false]
            },
            {
              "type": "logical",
              "attributes": {},
              "value": [false]
            },
            {
              "type": "logical",
              "attributes": {},
              "value": [true]
            },
            {
              "type": "logical",
              "attributes": {},
              "value": [false]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["cfMicro(A1, Cl / V, first = (Aa = Ka))\n\tC = A1 / V"]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Cl", "V", "Ka"]
                }
              },
              "value": [
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["StParmName", "Type", "State", "ThetaStParm", "OmegaStParm", "Covariates", "PMLStructure"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["StParm"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["Cl"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["LogNormal"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["Present"]
                    },
                    {
                      "type": "list",
                      "attributes": {
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Name", "InitialEstimates", "State", "Frozen", "StParmName", "PMLStructure"]
                        },
                        "class": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Theta"]
                        }
                      },
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["tvCl"]
                        },
                        {
                          "type": "list",
                          "attributes": {
                            "names": {
                              "type": "character",
                              "attributes": {},
                              "value": ["Lower", "Estimate", "Upper"]
                            },
                            "class": {
                              "type": "character",
                              "attributes": {},
                              "value": ["InitialEstimate"]
                            }
                          },
                          "value": [
                            {
                              "type": "logical",
                              "attributes": {},
                              "value": [null]
                            },
                            {
                              "type": "double",
                              "attributes": {},
                              "value": [1]
                            },
                            {
                              "type": "logical",
                              "attributes": {},
                              "value": [null]
                            }
                          ]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["Present"]
                        },
                        {
                          "type": "logical",
                          "attributes": {},
                          "value": [false]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["Cl"]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["PK1FOC"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Name", "InitialOmega", "State", "Frozen", "StParmName", "PMLStructure"]
                        },
                        "class": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Omega"]
                        }
                      },
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["nCl"]
                        },
                        {
                          "type": "double",
                          "attributes": {},
                          "value": [1]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["Present"]
                        },
                        {
                          "type": "logical",
                          "attributes": {},
                          "value": [false]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["Cl"]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["PK1FOC"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {},
                      "value": []
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PK1FOC"]
                    }
                  ]
                },
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["StParmName", "Type", "State", "ThetaStParm", "OmegaStParm", "Covariates", "PMLStructure"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["StParm"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["V"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["LogNormal"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["Present"]
                    },
                    {
                      "type": "list",
                      "attributes": {
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Name", "InitialEstimates", "State", "Frozen", "StParmName", "PMLStructure"]
                        },
                        "class": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Theta"]
                        }
                      },
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["tvV"]
                        },
                        {
                          "type": "list",
                          "attributes": {
                            "names": {
                              "type": "character",
                              "attributes": {},
                              "value": ["Lower", "Estimate", "Upper"]
                            },
                            "class": {
                              "type": "character",
                              "attributes": {},
                              "value": ["InitialEstimate"]
                            }
                          },
                          "value": [
                            {
                              "type": "logical",
                              "attributes": {},
                              "value": [null]
                            },
                            {
                              "type": "double",
                              "attributes": {},
                              "value": [1]
                            },
                            {
                              "type": "logical",
                              "attributes": {},
                              "value": [null]
                            }
                          ]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["Present"]
                        },
                        {
                          "type": "logical",
                          "attributes": {},
                          "value": [false]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["V"]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["PK1FOC"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Name", "InitialOmega", "State", "Frozen", "StParmName", "PMLStructure"]
                        },
                        "class": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Omega"]
                        }
                      },
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["nV"]
                        },
                        {
                          "type": "double",
                          "attributes": {},
                          "value": [1]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["Present"]
                        },
                        {
                          "type": "logical",
                          "attributes": {},
                          "value": [false]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["V"]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["PK1FOC"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {},
                      "value": []
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PK1FOC"]
                    }
                  ]
                },
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["StParmName", "Type", "State", "ThetaStParm", "OmegaStParm", "Covariates", "PMLStructure"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["StParm"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["Ka"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["LogNormal"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["Present"]
                    },
                    {
                      "type": "list",
                      "attributes": {
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Name", "InitialEstimates", "State", "Frozen", "StParmName", "PMLStructure"]
                        },
                        "class": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Theta"]
                        }
                      },
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["tvKa"]
                        },
                        {
                          "type": "list",
                          "attributes": {
                            "names": {
                              "type": "character",
                              "attributes": {},
                              "value": ["Lower", "Estimate", "Upper"]
                            },
                            "class": {
                              "type": "character",
                              "attributes": {},
                              "value": ["InitialEstimate"]
                            }
                          },
                          "value": [
                            {
                              "type": "logical",
                              "attributes": {},
                              "value": [null]
                            },
                            {
                              "type": "double",
                              "attributes": {},
                              "value": [1]
                            },
                            {
                              "type": "logical",
                              "attributes": {},
                              "value": [null]
                            }
                          ]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["Present"]
                        },
                        {
                          "type": "logical",
                          "attributes": {},
                          "value": [false]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["Ka"]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["PK1FOC"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Name", "InitialOmega", "State", "Frozen", "StParmName", "PMLStructure"]
                        },
                        "class": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Omega"]
                        }
                      },
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["nKa"]
                        },
                        {
                          "type": "double",
                          "attributes": {},
                          "value": [1]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["Present"]
                        },
                        {
                          "type": "logical",
                          "attributes": {},
                          "value": [false]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["Ka"]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["PK1FOC"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {},
                      "value": []
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PK1FOC"]
                    }
                  ]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CObs"]
                }
              },
              "value": [
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["ObservationName", "SigmasChosen", "BQL", "BQLValue", "Frozen", "ResetObs", "Covariates", "PMLStructure"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["Observation"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["CObs"]
                    },
                    {
                      "type": "list",
                      "attributes": {
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Additive", "LogAdditive", "Proportional", "AdditiveMultiplicative", "MixRatio", "Power", "ObservationName"]
                        },
                        "class": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Sigmas"]
                        }
                      },
                      "value": [
                        {
                          "type": "double",
                          "attributes": {},
                          "value": [0]
                        },
                        {
                          "type": "double",
                          "attributes": {},
                          "value": [0]
                        },
                        {
                          "type": "double",
                          "attributes": {},
                          "value": [0.1]
                        },
                        {
                          "type": "list",
                          "attributes": {
                            "names": {
                              "type": "character",
                              "attributes": {},
                              "value": ["AddPart", "PropPart"]
                            }
                          },
                          "value": [
                            {
                              "type": "double",
                              "attributes": {},
                              "value": [0]
                            },
                            {
                              "type": "list",
                              "attributes": {
                                "names": {
                                  "type": "character",
                                  "attributes": {},
                                  "value": ["CMultStdev"]
                                }
                              },
                              "value": [
                                {
                                  "type": "list",
                                  "attributes": {
                                    "names": {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["StParmName", "Type", "State", "ThetaStParm", "OmegaStParm", "Covariates", "PMLStructure"]
                                    },
                                    "class": {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["StParm"]
                                    }
                                  },
                                  "value": [
                                    {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["CMultStdev"]
                                    },
                                    {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["LogNormal"]
                                    },
                                    {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["None"]
                                    },
                                    {
                                      "type": "list",
                                      "attributes": {
                                        "names": {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["Name", "InitialEstimates", "State", "Frozen", "StParmName", "PMLStructure"]
                                        },
                                        "class": {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["Theta"]
                                        }
                                      },
                                      "value": [
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["tvCMultStdev"]
                                        },
                                        {
                                          "type": "list",
                                          "attributes": {
                                            "names": {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["Lower", "Estimate", "Upper"]
                                            },
                                            "class": {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["InitialEstimate"]
                                            }
                                          },
                                          "value": [
                                            {
                                              "type": "logical",
                                              "attributes": {},
                                              "value": [null]
                                            },
                                            {
                                              "type": "double",
                                              "attributes": {},
                                              "value": [0]
                                            },
                                            {
                                              "type": "logical",
                                              "attributes": {},
                                              "value": [null]
                                            }
                                          ]
                                        },
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["Present"]
                                        },
                                        {
                                          "type": "logical",
                                          "attributes": {},
                                          "value": [false]
                                        },
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["CMultStdev"]
                                        },
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["PK1FOC"]
                                        }
                                      ]
                                    },
                                    {
                                      "type": "list",
                                      "attributes": {
                                        "names": {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["Name", "InitialOmega", "State", "Frozen", "StParmName", "PMLStructure"]
                                        },
                                        "class": {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["Omega"]
                                        }
                                      },
                                      "value": [
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["nCMultStdev"]
                                        },
                                        {
                                          "type": "double",
                                          "attributes": {},
                                          "value": [1]
                                        },
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["None"]
                                        },
                                        {
                                          "type": "logical",
                                          "attributes": {},
                                          "value": [false]
                                        },
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["CMultStdev"]
                                        },
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["PK1FOC"]
                                        }
                                      ]
                                    },
                                    {
                                      "type": "list",
                                      "attributes": {},
                                      "value": []
                                    },
                                    {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["PK1FOC"]
                                    }
                                  ]
                                }
                              ]
                            }
                          ]
                        },
                        {
                          "type": "list",
                          "attributes": {
                            "names": {
                              "type": "character",
                              "attributes": {},
                              "value": ["AddPart", "PropPart"]
                            }
                          },
                          "value": [
                            {
                              "type": "double",
                              "attributes": {},
                              "value": [0]
                            },
                            {
                              "type": "list",
                              "attributes": {
                                "names": {
                                  "type": "character",
                                  "attributes": {},
                                  "value": ["CMixRatio"]
                                }
                              },
                              "value": [
                                {
                                  "type": "list",
                                  "attributes": {
                                    "names": {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["StParmName", "Type", "State", "ThetaStParm", "OmegaStParm", "Covariates", "PMLStructure"]
                                    },
                                    "class": {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["StParm"]
                                    }
                                  },
                                  "value": [
                                    {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["CMixRatio"]
                                    },
                                    {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["LogNormal"]
                                    },
                                    {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["None"]
                                    },
                                    {
                                      "type": "list",
                                      "attributes": {
                                        "names": {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["Name", "InitialEstimates", "State", "Frozen", "StParmName", "PMLStructure"]
                                        },
                                        "class": {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["Theta"]
                                        }
                                      },
                                      "value": [
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["tvCMixRatio"]
                                        },
                                        {
                                          "type": "list",
                                          "attributes": {
                                            "names": {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["Lower", "Estimate", "Upper"]
                                            },
                                            "class": {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["InitialEstimate"]
                                            }
                                          },
                                          "value": [
                                            {
                                              "type": "logical",
                                              "attributes": {},
                                              "value": [null]
                                            },
                                            {
                                              "type": "double",
                                              "attributes": {},
                                              "value": [0]
                                            },
                                            {
                                              "type": "logical",
                                              "attributes": {},
                                              "value": [null]
                                            }
                                          ]
                                        },
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["Present"]
                                        },
                                        {
                                          "type": "logical",
                                          "attributes": {},
                                          "value": [false]
                                        },
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["CMixRatio"]
                                        },
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["PK1FOC"]
                                        }
                                      ]
                                    },
                                    {
                                      "type": "list",
                                      "attributes": {
                                        "names": {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["Name", "InitialOmega", "State", "Frozen", "StParmName", "PMLStructure"]
                                        },
                                        "class": {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["Omega"]
                                        }
                                      },
                                      "value": [
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["nCMixRatio"]
                                        },
                                        {
                                          "type": "double",
                                          "attributes": {},
                                          "value": [1]
                                        },
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["None"]
                                        },
                                        {
                                          "type": "logical",
                                          "attributes": {},
                                          "value": [false]
                                        },
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["CMixRatio"]
                                        },
                                        {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["PK1FOC"]
                                        }
                                      ]
                                    },
                                    {
                                      "type": "list",
                                      "attributes": {},
                                      "value": []
                                    },
                                    {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["PK1FOC"]
                                    }
                                  ]
                                }
                              ]
                            }
                          ]
                        },
                        {
                          "type": "list",
                          "attributes": {
                            "names": {
                              "type": "character",
                              "attributes": {},
                              "value": ["PowerPart", "StdevPart"]
                            }
                          },
                          "value": [
                            {
                              "type": "double",
                              "attributes": {},
                              "value": [0]
                            },
                            {
                              "type": "double",
                              "attributes": {},
                              "value": [0]
                            }
                          ]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["CObs"]
                        }
                      ]
                    },
                    {
                      "type": "logical",
                      "attributes": {},
                      "value": [false]
                    },
                    {
                      "type": "logical",
                      "attributes": {},
                      "value": [null]
                    },
                    {
                      "type": "logical",
                      "attributes": {},
                      "value": [false]
                    },
                    {
                      "type": "logical",
                      "attributes": {},
                      "value": [false]
                    },
                    {
                      "type": "list",
                      "attributes": {},
                      "value": []
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PK1FOC"]
                    }
                  ]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Aa"]
                }
              },
              "value": [
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["DosepointName", "State", "tlag", "bioavail", "duration", "rate", "PMLStructure"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["Dosepoint"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["Aa"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["Present"]
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PK1FOC"]
                    }
                  ]
                }
              ]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["Type", "PMLCode", "TimeBased", "Responses", "CustomCovariates", "CustomDosepoints", "CustomStParms", "CustomFixefs", "CustomRanefs", "CFs", "CustomDerivs", "Transits", "CustomUrines", "SpaceName", "StParms"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["CustomSpace"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["Custom"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["deriv(Aa1 = -Ktr * Aa1)\n\t\tderiv(Aa2 = Ktr * (Aa1 - Aa2))\n\t\tderiv(A1 = Ktr * Aa2 - Cl * C)\n\t\tdosepoint(Aa1)\n\t\tC = A1 / V\n\t\terror(CEps = 0.1)\n\t\tobserve(CObs = C * (1 + CEps))\n\t\tfcovariate(OCC())\n\t\t\n\t\tstparm(Cl = tvCl * exp(nCl))\n\t\tstparm(Ktr = tvKtr * exp(nKtr))\n\t\t\n\t\tfixef(tvCl = c(, 1, ))\n\t\tfixef(tvKtr = c(, 1, ))\n\t\t\n\t\t\n\t\tranef(diag(nCl) = c(1))\n\t\tranef(diag(nKtr) = c(1))"]
            },
            {
              "type": "logical",
              "attributes": {},
              "value": [true]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["CObs"]
                }
              },
              "value": [
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["ObservationName", "Type", "Statement", "StatementNames", "Sigma", "Dobefore", "Doafter", "BQL", "BQLValue", "PMLStructure"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["ObservationCustom"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["CObs"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["observe"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["(CObs=C*(1+CEps))"]
                    },
                    {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["C", "CEps"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": ["CEps"]
                        }
                      },
                      "value": [
                        {
                          "type": "character",
                          "attributes": {
                            "names": {
                              "type": "character",
                              "attributes": {},
                              "value": ["ErrorValue"]
                            }
                          },
                          "value": ["0.1"]
                        }
                      ]
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "logical",
                      "attributes": {},
                      "value": [false]
                    },
                    {
                      "type": "logical",
                      "attributes": {},
                      "value": [null]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["l520"]
                    }
                  ]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["OCC"]
                }
              },
              "value": [
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["Name", "Direction", "Type", "Statement", "PMLStructure"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["CovariateCustom"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["OCC"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["Forward"]
                    },
                    {
                      "type": "character",
                      "attributes": {
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": ["IsCategorical"]
                        }
                      },
                      "value": ["Categorical"]
                    },
                    {
                      "type": "character",
                      "attributes": {
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Statement"]
                        }
                      },
                      "value": ["fcovariate(OCC())"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["l520"]
                    }
                  ]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Aa1"]
                }
              },
              "value": [
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["DosepointName", "DoseType", "dobefore", "doafter", "rate", "duration", "idosevar", "infdosevar", "infratevar", "bioavail", "tlag", "Statement", "PMLStructure"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["DosepointCustom"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["Aa1"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["dosepoint"]
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "NULL"
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["(Aa1)"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["l520"]
                    }
                  ]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Cl", "Ktr"]
                }
              },
              "value": [
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["StParmName", "Statement", "PMLStructure"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["StParmCustom"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["Cl"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["(Cl = tvCl * exp(nCl))"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["l520"]
                    }
                  ]
                },
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["StParmName", "Statement", "PMLStructure"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["StParmCustom"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["Ktr"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["(Ktr = tvKtr * exp(nKtr))"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["l520"]
                    }
                  ]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["tvCl", "tvKtr"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["Statement", "Type"]
                    }
                  },
                  "value": ["(tvCl = c(, 1, ))", "fixef"]
                },
                {
                  "type": "character",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["Statement", "Type"]
                    }
                  },
                  "value": ["(tvKtr = c(, 1, ))", "fixef"]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["nV", "nVx0", "nCl", "nKtr"]
                }
              },
              "value": [
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["RanefType", "RanefNames", "RanefValues", "Statement", "Type"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["diag"]
                    },
                    {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["nV"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["1"]
                        }
                      ]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["diag(nV)=c(1)"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["ranef"]
                    }
                  ]
                },
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["RanefType", "RanefNames", "RanefValues", "Statement", "Type"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["diag"]
                    },
                    {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["nVx0"]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["nVx1"]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["nVx2"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["1"]
                        }
                      ]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["diag(nVx0)=c(1),same(nVx1),same(nVx2)"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["ranef"]
                    }
                  ]
                },
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["RanefType", "RanefNames", "RanefValues", "Statement", "Type"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["diag"]
                    },
                    {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["nCl"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["1"]
                        }
                      ]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["diag(nCl)=c(1)"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["ranef"]
                    }
                  ]
                },
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["RanefType", "RanefNames", "RanefValues", "Statement", "Type"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["diag"]
                    },
                    {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["nKtr"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["1"]
                        }
                      ]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["diag(nKtr)=c(1)"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["ranef"]
                    }
                  ]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {},
              "value": []
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Aa1", "Aa2", "A1"]
                }
              },
              "value": [
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["Statement", "StatementNames", "Type"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["(Aa1=-Ktr*Aa1)"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["-Ktr"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["deriv"]
                    }
                  ]
                },
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["Statement", "StatementNames", "Type"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["(Aa2=Ktr*(Aa1-Aa2))"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["Ktr", "Aa1-Aa2"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["deriv"]
                    }
                  ]
                },
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["Statement", "StatementNames", "Type"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["(A1=Ktr*Aa2-Cl*C)"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["Ktr", "Aa2-Cl", "C"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["deriv"]
                    }
                  ]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {},
              "value": []
            },
            {
              "type": "list",
              "attributes": {},
              "value": []
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["l520"]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["V"]
                }
              },
              "value": [
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["StParmName", "Type", "State", "ThetaStParm", "OmegaStParm", "Covariates", "PMLStructure"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["StParm"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["V"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["LogNormal2"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["Present"]
                    },
                    {
                      "type": "list",
                      "attributes": {
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Name", "InitialEstimates", "State", "Frozen", "StParmName", "PMLStructure"]
                        },
                        "class": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Theta"]
                        }
                      },
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["tvV"]
                        },
                        {
                          "type": "list",
                          "attributes": {
                            "names": {
                              "type": "character",
                              "attributes": {},
                              "value": ["Lower", "Estimate", "Upper"]
                            },
                            "class": {
                              "type": "character",
                              "attributes": {},
                              "value": ["InitialEstimate"]
                            }
                          },
                          "value": [
                            {
                              "type": "logical",
                              "attributes": {},
                              "value": [null]
                            },
                            {
                              "type": "double",
                              "attributes": {},
                              "value": [1]
                            },
                            {
                              "type": "logical",
                              "attributes": {},
                              "value": [null]
                            }
                          ]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["Present"]
                        },
                        {
                          "type": "logical",
                          "attributes": {},
                          "value": [false]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["V"]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["l520"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Name", "InitialOmega", "State", "Frozen", "StParmName", "PMLStructure"]
                        },
                        "class": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Omega"]
                        }
                      },
                      "value": [
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["nV"]
                        },
                        {
                          "type": "double",
                          "attributes": {},
                          "value": [1]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["Present"]
                        },
                        {
                          "type": "logical",
                          "attributes": {},
                          "value": [false]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["V"]
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["l520"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {},
                      "value": []
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["l520"]
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }

