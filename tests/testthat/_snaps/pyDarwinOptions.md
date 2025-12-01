# create_pyDarwinOptions GA returns a valid list

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["author", "algorithm", "GA", "random_seed", "num_parallel", "num_generations", "population_size", "crash_value", "penalty", "downhill_period", "num_niches", "niche_radius", "local_2_bit_search", "final_downhill_search", "local_grid_search", "model_run_timeout", "model_run_priority_class", "keep_key_models", "keep_best_models", "rerun_key_models", "rerun_front_models", "use_saved_models", "remove_run_dir", "remove_temp_dir", "keep_files", "use_system_options", "model_cache", "model_run_man", "engine_adapter", "skip_running", "output_dir", "key_models_dir", "non_dominated_models_dir", "nlme_dir"]
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
          "type": "logical",
          "attributes": {},
          "value": [false]
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
          "type": "character",
          "attributes": {},
          "value": ["dmp.txt", "posthoc.csv"]
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
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/output"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/key_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/non_dominated_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["~/InstallDirNLME/"]
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
          "value": ["author", "algorithm", "random_seed", "num_parallel", "exhaustive_batch_size", "crash_value", "penalty", "model_run_timeout", "model_run_priority_class", "keep_key_models", "use_saved_models", "remove_run_dir", "remove_temp_dir", "use_system_options", "model_cache", "model_run_man", "engine_adapter", "skip_running", "output_dir", "key_models_dir", "non_dominated_models_dir", "nmfe_path"]
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
          "value": ["nonmem"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/output"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/key_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/non_dominated_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["path/to/nmfe_placeholder"]
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
          "value": ["author", "algorithm", "random_seed", "num_parallel", "num_generations", "population_size", "num_opt_chains", "crash_value", "penalty", "downhill_period", "num_niches", "niche_radius", "local_2_bit_search", "final_downhill_search", "local_grid_search", "model_run_timeout", "model_run_priority_class", "keep_key_models", "keep_best_models", "rerun_key_models", "rerun_front_models", "use_saved_models", "remove_run_dir", "remove_temp_dir", "keep_files", "use_system_options", "model_cache", "model_run_man", "engine_adapter", "skip_running", "output_dir", "key_models_dir", "non_dominated_models_dir", "nlme_dir"]
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
          "type": "logical",
          "attributes": {},
          "value": [false]
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
          "type": "character",
          "attributes": {},
          "value": ["dmp.txt", "posthoc.csv"]
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
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/output"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/key_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/non_dominated_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["~/InstallDirNLME/"]
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
          "value": ["author", "algorithm", "random_seed", "num_parallel", "num_generations", "population_size", "num_opt_chains", "crash_value", "penalty", "downhill_period", "num_niches", "niche_radius", "local_2_bit_search", "final_downhill_search", "local_grid_search", "model_run_timeout", "model_run_priority_class", "keep_key_models", "keep_best_models", "rerun_key_models", "rerun_front_models", "use_saved_models", "remove_run_dir", "remove_temp_dir", "keep_files", "use_system_options", "model_cache", "model_run_man", "engine_adapter", "skip_running", "output_dir", "key_models_dir", "non_dominated_models_dir", "nlme_dir"]
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
          "type": "logical",
          "attributes": {},
          "value": [false]
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
          "type": "character",
          "attributes": {},
          "value": ["dmp.txt", "posthoc.csv"]
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
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/output"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/key_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/non_dominated_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["~/InstallDirNLME/"]
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
          "value": ["author", "algorithm", "random_seed", "num_parallel", "num_generations", "population_size", "num_opt_chains", "crash_value", "penalty", "downhill_period", "num_niches", "niche_radius", "local_2_bit_search", "final_downhill_search", "local_grid_search", "model_run_timeout", "model_run_priority_class", "keep_key_models", "keep_best_models", "rerun_key_models", "rerun_front_models", "use_saved_models", "remove_run_dir", "remove_temp_dir", "keep_files", "use_system_options", "model_cache", "model_run_man", "engine_adapter", "skip_running", "output_dir", "key_models_dir", "non_dominated_models_dir", "nlme_dir"]
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
          "type": "logical",
          "attributes": {},
          "value": [false]
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
          "type": "character",
          "attributes": {},
          "value": ["dmp.txt", "posthoc.csv"]
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
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/output"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/key_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/non_dominated_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["~/InstallDirNLME/"]
        }
      ]
    }

# create_pyDarwinOptions MOGA NSGA-II default

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["author", "algorithm", "MOGA", "random_seed", "num_parallel", "num_generations", "population_size", "crash_value", "effect_limit", "downhill_period", "num_niches", "niche_radius", "final_downhill_search", "local_grid_search", "model_run_timeout", "model_run_priority_class", "rerun_front_models", "use_saved_models", "remove_run_dir", "remove_temp_dir", "use_system_options", "model_cache", "model_run_man", "engine_adapter", "skip_running", "output_dir", "non_dominated_models_dir", "nmfe_path"]
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
          "value": ["MOGA"]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["objectives", "constraints", "partitions", "crossover", "crossover_rate", "mutation_rate", "attribute_mutation_probability"]
            }
          },
          "value": [
            {
              "type": "double",
              "attributes": {},
              "value": [3]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [12]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["single"]
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
          "value": [4]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [99999999]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [5]
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
          "value": [false]
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
          "value": ["nonmem"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/output"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/non_dominated_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["nmfe75"]
        }
      ]
    }

# create_pyDarwinOptions MOGA NSGA-III with postprocessing and constraints

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["author", "algorithm", "MOGA", "random_seed", "num_parallel", "num_generations", "population_size", "crash_value", "downhill_period", "num_niches", "niche_radius", "final_downhill_search", "local_grid_search", "model_run_timeout", "model_run_priority_class", "postprocess", "rerun_front_models", "use_saved_models", "remove_run_dir", "remove_temp_dir", "keep_files", "use_system_options", "model_cache", "model_run_man", "engine_adapter", "skip_running", "output_dir", "non_dominated_models_dir", "nlme_dir"]
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
          "value": ["MOGA3"]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["objectives", "constraints", "partitions", "crossover", "crossover_rate", "mutation_rate", "attribute_mutation_probability"]
            }
          },
          "value": [
            {
              "type": "double",
              "attributes": {},
              "value": [3]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [10]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["single"]
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
          "value": [60]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [99999999]
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
          "value": [false]
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
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["use_r", "post_run_r_code", "r_timeout"]
            }
          },
          "value": [
            {
              "type": "logical",
              "attributes": {},
              "value": [true]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["test.R"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [30]
            }
          ]
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
          "type": "character",
          "attributes": {},
          "value": ["dmp.txt", "posthoc.csv"]
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
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/output"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/non_dominated_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["~/InstallDirNLME/"]
        }
      ]
    }

# create_pyDarwinOptions new general options and interactions

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["author", "algorithm", "GA", "random_seed", "num_parallel", "num_generations", "population_size", "crash_value", "penalty", "effect_limit", "downhill_period", "num_niches", "niche_radius", "local_2_bit_search", "final_downhill_search", "local_grid_search", "max_local_grid_search_bits", "search_omega_bands", "individual_omega_search", "search_omega_sub_matrix", "max_omega_sub_matrix", "model_run_timeout", "model_run_priority_class", "keep_key_models", "keep_best_models", "rerun_key_models", "rerun_front_models", "use_saved_models", "remove_run_dir", "remove_temp_dir", "keep_files", "keep_extensions", "use_system_options", "model_cache", "model_run_man", "engine_adapter", "skip_running", "output_dir", "key_models_dir", "non_dominated_models_dir", "nmfe_path", "my_custom_darwin_param"]
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
          "value": [6]
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
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [3]
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
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2]
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
          "type": "character",
          "attributes": {},
          "value": ["sim.ext"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["grd", "phi"]
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
          "value": ["nonmem"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/output"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/key_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["ndm_dir"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["nmfe75"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["test_value"]
        }
      ]
    }

# create_pyDarwinOptions nlme omega search

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["author", "algorithm", "GA", "random_seed", "num_parallel", "num_generations", "population_size", "crash_value", "penalty", "downhill_period", "num_niches", "niche_radius", "local_2_bit_search", "final_downhill_search", "local_grid_search", "search_omega_blocks", "individual_omega_search", "model_run_timeout", "model_run_priority_class", "keep_key_models", "keep_best_models", "rerun_key_models", "rerun_front_models", "use_saved_models", "remove_run_dir", "remove_temp_dir", "keep_files", "use_system_options", "model_cache", "model_run_man", "engine_adapter", "skip_running", "output_dir", "key_models_dir", "non_dominated_models_dir", "nlme_dir"]
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
          "type": "character",
          "attributes": {},
          "value": ["dmp.txt", "posthoc.csv"]
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
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/output"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/key_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["{working_dir}/non_dominated_models"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["~/InstallDirNLME/"]
        }
      ]
    }

