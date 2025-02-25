# add_Covariate function tests

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["PK1IVC"]
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
              "value": ["Intravenous"]
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
              "value": ["cfMicro(A1, Cl / V)\n\tC = A1 / V"]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Cl", "V"]
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
                          "value": ["PK1IVC"]
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
                          "value": ["PK1IVC"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": ["WT", "Age"]
                        }
                      },
                      "value": [
                        {
                          "type": "list",
                          "attributes": {
                            "names": {
                              "type": "character",
                              "attributes": {},
                              "value": ["Name", "Type", "StParmName", "State", "Direction", "Center", "Categories", "Thetas", "Omegas", "PMLStructure"]
                            },
                            "class": {
                              "type": "character",
                              "attributes": {},
                              "value": ["Covariate"]
                            }
                          },
                          "value": [
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["WT"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["Continuous"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["Cl"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["Present"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["Forward"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["None"]
                            },
                            {
                              "type": "NULL"
                            },
                            {
                              "type": "list",
                              "attributes": {
                                "names": {
                                  "type": "character",
                                  "attributes": {},
                                  "value": ["dCldWT"]
                                }
                              },
                              "value": [
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
                                      "value": ["dCldWT"]
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
                                      "value": ["Cl"]
                                    },
                                    {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["PK1IVC"]
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
                              "type": "character",
                              "attributes": {},
                              "value": ["PK1IVC"]
                            }
                          ]
                        },
                        {
                          "type": "list",
                          "attributes": {
                            "names": {
                              "type": "character",
                              "attributes": {},
                              "value": ["Name", "Type", "StParmName", "State", "Direction", "Center", "Categories", "Thetas", "Omegas", "PMLStructure"]
                            },
                            "class": {
                              "type": "character",
                              "attributes": {},
                              "value": ["Covariate"]
                            }
                          },
                          "value": [
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["Age"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["Categorical"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["Cl"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["Present"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["Backward"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["None"]
                            },
                            {
                              "type": "double",
                              "attributes": {},
                              "value": [1, 2, 3]
                            },
                            {
                              "type": "list",
                              "attributes": {
                                "names": {
                                  "type": "character",
                                  "attributes": {},
                                  "value": ["dCldAge2", "dCldAge3"]
                                }
                              },
                              "value": [
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
                                      "value": ["dCldAge2"]
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
                                      "value": ["Cl"]
                                    },
                                    {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["PK1IVC"]
                                    }
                                  ]
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
                                      "value": ["dCldAge3"]
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
                                      "value": ["Cl"]
                                    },
                                    {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["PK1IVC"]
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
                              "type": "character",
                              "attributes": {},
                              "value": ["PK1IVC"]
                            }
                          ]
                        }
                      ]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PK1IVC"]
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
                          "value": ["PK1IVC"]
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
                          "value": ["PK1IVC"]
                        }
                      ]
                    },
                    {
                      "type": "list",
                      "attributes": {
                        "names": {
                          "type": "character",
                          "attributes": {},
                          "value": ["WT"]
                        }
                      },
                      "value": [
                        {
                          "type": "list",
                          "attributes": {
                            "names": {
                              "type": "character",
                              "attributes": {},
                              "value": ["Name", "Type", "StParmName", "State", "Direction", "Center", "Categories", "Thetas", "Omegas", "PMLStructure"]
                            },
                            "class": {
                              "type": "character",
                              "attributes": {},
                              "value": ["Covariate"]
                            }
                          },
                          "value": [
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["WT"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["Continuous"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["V"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["Present"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["Forward"]
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["None"]
                            },
                            {
                              "type": "NULL"
                            },
                            {
                              "type": "list",
                              "attributes": {
                                "names": {
                                  "type": "character",
                                  "attributes": {},
                                  "value": ["dVdWT"]
                                }
                              },
                              "value": [
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
                                      "value": ["dVdWT"]
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
                                      "value": ["V"]
                                    },
                                    {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["PK1IVC"]
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
                              "type": "character",
                              "attributes": {},
                              "value": ["PK1IVC"]
                            }
                          ]
                        }
                      ]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PK1IVC"]
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
                                          "value": ["PK1IVC"]
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
                                          "value": ["PK1IVC"]
                                        }
                                      ]
                                    },
                                    {
                                      "type": "list",
                                      "attributes": {
                                        "names": {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["WT"]
                                        }
                                      },
                                      "value": [
                                        {
                                          "type": "list",
                                          "attributes": {
                                            "names": {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["Name", "Type", "StParmName", "State", "Direction", "Center", "Categories", "Thetas", "Omegas", "PMLStructure"]
                                            },
                                            "class": {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["Covariate"]
                                            }
                                          },
                                          "value": [
                                            {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["WT"]
                                            },
                                            {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["Continuous"]
                                            },
                                            {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["CMultStdev"]
                                            },
                                            {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["Present"]
                                            },
                                            {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["Forward"]
                                            },
                                            {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["None"]
                                            },
                                            {
                                              "type": "NULL"
                                            },
                                            {
                                              "type": "list",
                                              "attributes": {
                                                "names": {
                                                  "type": "character",
                                                  "attributes": {},
                                                  "value": ["dCMultStdevdWT"]
                                                }
                                              },
                                              "value": [
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
                                                      "value": ["dCMultStdevdWT"]
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
                                                      "value": ["PK1IVC"]
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
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["PK1IVC"]
                                            }
                                          ]
                                        }
                                      ]
                                    },
                                    {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["PK1IVC"]
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
                                          "value": ["PK1IVC"]
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
                                          "value": ["PK1IVC"]
                                        }
                                      ]
                                    },
                                    {
                                      "type": "list",
                                      "attributes": {
                                        "names": {
                                          "type": "character",
                                          "attributes": {},
                                          "value": ["WT"]
                                        }
                                      },
                                      "value": [
                                        {
                                          "type": "list",
                                          "attributes": {
                                            "names": {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["Name", "Type", "StParmName", "State", "Direction", "Center", "Categories", "Thetas", "Omegas", "PMLStructure"]
                                            },
                                            "class": {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["Covariate"]
                                            }
                                          },
                                          "value": [
                                            {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["WT"]
                                            },
                                            {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["Continuous"]
                                            },
                                            {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["CMixRatio"]
                                            },
                                            {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["Present"]
                                            },
                                            {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["Forward"]
                                            },
                                            {
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["None"]
                                            },
                                            {
                                              "type": "NULL"
                                            },
                                            {
                                              "type": "list",
                                              "attributes": {
                                                "names": {
                                                  "type": "character",
                                                  "attributes": {},
                                                  "value": ["dCMixRatiodWT"]
                                                }
                                              },
                                              "value": [
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
                                                      "value": ["dCMixRatiodWT"]
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
                                                      "value": ["PK1IVC"]
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
                                              "type": "character",
                                              "attributes": {},
                                              "value": ["PK1IVC"]
                                            }
                                          ]
                                        }
                                      ]
                                    },
                                    {
                                      "type": "character",
                                      "attributes": {},
                                      "value": ["PK1IVC"]
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
                      "value": ["PK1IVC"]
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
                  "value": ["A1"]
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
                      "value": ["A1"]
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
                      "value": ["PK1IVC"]
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }

---

    PK1IVC 
     test() {
    	cfMicro(A1, Cl / V)
    	C = A1 / V
    	dosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)
    	error(CEps = 0.1)
    	observe(CObs = C * (1 + CEps))
    	fcovariate(BW)
    	stparm(Cl = tvCl * exp( nCl ))
    	fixef(tvCl= c(, 5, ))
    	ranef(diag(nCl) = c(1))
    	stparm(V = tvV  * (BW/70)^dVdBW * exp( nV ))
    	fixef(tvV= c(, 5, ))
    	fixef(dVdBW= c(, 0, ))
    	ranef(diag(nV) = c(1))
    
    } 
    PK2IVC 
     test() {
    	cfMicro(A1, Cl / V, Cl2 / V, Cl2 / V2)
    C = A1 / V
    	dosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)
    	error(CEps = 0.1)
    	observe(CObs = C * (1 + CEps))
    	fcovariate(BW)
    	fcovariate(Sex())
    	stparm(Cl = tvCl * exp( nCl ))
    	fixef(tvCl= c(, 5, ))
    	ranef(diag(nCl) = c(1))
    	stparm(V = tvV  * (BW/70)^dVdBW * exp( nV ))
    	fixef(tvV= c(, 5, ))
    	fixef(dVdBW= c(, 0, ))
    	ranef(diag(nV) = c(1))
    	stparm(Cl2 = tvCl2 * exp( nCl2 ))
    	fixef(tvCl2= c(, 1, ))
    	ranef(diag(nCl2) = c(1))
    	stparm(V2 = tvV2 {V2_Sex[1]} * exp( nV2 ))
    	fixef(tvV2= c(, 1, ))
    	{V2_Sex[2]}
    	ranef(diag(nV2) = c(1))
    
    } 
    PK3IVC 
     test() {
    	cfMicro(A1, Cl / V, Cl2 / V, Cl2 / V2, Cl3 / V, Cl3 / V3)
    	C = A1 / V
    	dosepoint(A1, idosevar = A1Dose, infdosevar = A1InfDose, infratevar = A1InfRate)
    	error(CEps = 0.1)
    	observe(CObs = C * (1 + CEps))
    	fcovariate(BW)
    	fcovariate(Sex())
    	fcovariate(Occasion())
    	stparm(Cl = tvCl * exp( nCl ))
    	fixef(tvCl= c(, 5, ))
    	ranef(diag(nCl) = c(1))
    	stparm(V = tvV  * (BW/70)^dVdBW * exp( nV ))
    	fixef(tvV= c(, 5, ))
    	fixef(dVdBW= c(, 0, ))
    	ranef(diag(nV) = c(1))
    	stparm(Cl2 = tvCl2 * exp( nCl2 ))
    	fixef(tvCl2= c(, 1, ))
    	ranef(diag(nCl2) = c(1))
    	stparm(V2 = tvV2 {V2_Sex[1]} * exp( nV2 ))
    	fixef(tvV2= c(, 1, ))
    	{V2_Sex[2]}
    	ranef(diag(nV2) = c(1))
    	stparm(Cl3 = tvCl3 * exp( nCl3 ))
    	fixef(tvCl3= c(, 5, ))
    	ranef(diag(nCl3) = c(1))
    	stparm(V3 = tvV3 * exp( nV3  + (Occasion==1)*nV3Occasionx1 + (Occasion==2)*nV3Occasionx2 + (Occasion==3)*nV3Occasionx3 ))
    	fixef(tvV3= c(, 1, ))
    	ranef(diag(nV3) = c(1))
    	ranef(diag(nV3Occasionx1) = c(1) , same(nV3Occasionx2) , same(nV3Occasionx3))
    
    } 

# remove_Covariate function tests

    {
      "type": "NULL"
    }

