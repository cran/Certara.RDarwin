# create_pyDarwinOptions returns a valid list

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["author", "algorithm", "GA", "random_seed", "num_parallel", "num_generations", "population_size", "crash_value", "penalty", "downhill_period", "num_niches", "niche_radius", "local_2_bit_search", "final_downhill_search", "model_run_timeout", "model_run_priority_class", "keep_key_models", "use_saved_models", "saved_models_readonly", "remove_run_dir", "remove_temp_dir", "use_system_options", "model_cache", "model_run_man", "engine_adapter", "output_dir", "nlme_dir", "rscript_path"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["John Doe"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["GA"]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["elitist_num", "crossover_rate", "mutation_rate", "sharing_alpha", "selection", "selection_size", "crossover_operator", "mutate", "attribute_mutation_probability", "niche_penalty"]
            }
          },
          "value": [
            {
              "type": "double",
              "attributes": {},
              "value": [2]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.95]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.95]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.1]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["tournament"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [2]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["cxOnePoint"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["flipBit"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.1]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [20]
            }
          ]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [11]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [6]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [10]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [99999999]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["theta", "omega", "sigma", "convergence", "covariance", "correlation", "condition_number", "non_influential_tokens"]
            }
          },
          "value": [
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1e-05]
            }
          ]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1200]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["below_normal"]
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
          "value": [true]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["darwin.MemoryModelCache"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["darwin.LocalRunManager"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["nlme"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/output"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["C:/Program Files/Certara/NLME_Engine"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Rscript"]
        }
      ]
    }

# create_pyDarwinOptions EX

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["author", "algorithm", "num_parallel", "exhaustive_batch_size", "crash_value", "penalty", "model_run_timeout", "model_run_priority_class", "keep_key_models", "use_saved_models", "saved_models_readonly", "remove_run_dir", "remove_temp_dir", "use_system_options", "model_cache", "model_run_man", "engine_adapter", "output_dir", "nlme_dir", "rscript_path"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": [""]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["EX"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [100]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [99999999]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["theta", "omega", "sigma", "convergence", "covariance", "correlation", "condition_number", "non_influential_tokens"]
            }
          },
          "value": [
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1e-05]
            }
          ]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1200]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["below_normal"]
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
          "value": [true]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["darwin.MemoryModelCache"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["darwin.LocalRunManager"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["nlme"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/output"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["C:/Program Files/Certara/NLME_Engine"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Rscript"]
        }
      ]
    }

# create_pyDarwinOptions GBRT

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["author", "algorithm", "random_seed", "num_parallel", "num_generations", "population_size", "num_opt_chains", "crash_value", "penalty", "downhill_period", "num_niches", "niche_radius", "local_2_bit_search", "final_downhill_search", "model_run_timeout", "model_run_priority_class", "keep_key_models", "use_saved_models", "saved_models_readonly", "remove_run_dir", "remove_temp_dir", "use_system_options", "model_cache", "model_run_man", "engine_adapter", "output_dir", "nlme_dir", "rscript_path"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": [""]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["GBRT"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [11]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [6]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [99999999]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["theta", "omega", "sigma", "convergence", "covariance", "correlation", "condition_number", "non_influential_tokens"]
            }
          },
          "value": [
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1e-05]
            }
          ]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1200]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["below_normal"]
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
          "value": [true]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["darwin.MemoryModelCache"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["darwin.LocalRunManager"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["nlme"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/output"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["C:/Program Files/Certara/NLME_Engine"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Rscript"]
        }
      ]
    }

# create_pyDarwinOptions RF

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["author", "algorithm", "random_seed", "num_parallel", "num_generations", "population_size", "num_opt_chains", "crash_value", "penalty", "downhill_period", "num_niches", "niche_radius", "local_2_bit_search", "final_downhill_search", "model_run_timeout", "model_run_priority_class", "keep_key_models", "use_saved_models", "saved_models_readonly", "remove_run_dir", "remove_temp_dir", "use_system_options", "model_cache", "model_run_man", "engine_adapter", "output_dir", "nlme_dir", "rscript_path"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": [""]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["RF"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [11]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [6]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [99999999]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["theta", "omega", "sigma", "convergence", "covariance", "correlation", "condition_number", "non_influential_tokens"]
            }
          },
          "value": [
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1e-05]
            }
          ]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1200]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["below_normal"]
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
          "value": [true]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["darwin.MemoryModelCache"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["darwin.LocalRunManager"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["nlme"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/output"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["C:/Program Files/Certara/NLME_Engine"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Rscript"]
        }
      ]
    }

# create_pyDarwinOptions GP

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["author", "algorithm", "random_seed", "num_parallel", "num_generations", "population_size", "num_opt_chains", "crash_value", "penalty", "downhill_period", "num_niches", "niche_radius", "local_2_bit_search", "final_downhill_search", "model_run_timeout", "model_run_priority_class", "keep_key_models", "use_saved_models", "saved_models_readonly", "remove_run_dir", "remove_temp_dir", "use_system_options", "model_cache", "model_run_man", "engine_adapter", "output_dir", "nlme_dir", "rscript_path"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": [""]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["GP"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [11]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [6]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [4]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [99999999]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["theta", "omega", "sigma", "convergence", "covariance", "correlation", "condition_number", "non_influential_tokens"]
            }
          },
          "value": [
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [100]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1e-05]
            }
          ]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1200]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["below_normal"]
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
          "value": [true]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["darwin.MemoryModelCache"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["darwin.LocalRunManager"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["nlme"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/output"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["C:/Program Files/Certara/NLME_Engine"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Rscript"]
        }
      ]
    }

