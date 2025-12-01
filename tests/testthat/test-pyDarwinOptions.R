# Test create_pyDarwinOptions()
test_that("create_pyDarwinOptions GA returns a valid list", {
  pyDarwinOptions <-
    create_pyDarwinOptions(
      author = "John Doe",
      algorithm = "GA",
      population_size = 10,
      rscript_path = "Rscript",
      nlme_dir = "~/InstallDirNLME/",
      postprocess = pyDarwinOptionsPostprocess()
    )

  pyDarwinOptions$gcc_dir <- NULL # OS dependent
  pyDarwinOptions$rscript_path <- NULL # Path normalization dependent
  expect_snapshot_value(pyDarwinOptions, style = "json2")
})

test_that("create_pyDarwinOptions EX", {
  pyDarwinOptions <-
    create_pyDarwinOptions(
      algorithm = "EX",
      engine_adapter = "nonmem", # To test random_seed logic with nonmem
      nmfe_path = "path/to/nmfe_placeholder", # Added to prevent warning
      search_omega_bands = FALSE, # Ensure random_seed is NULLed
      rscript_path = "Rscript"
    )

  pyDarwinOptions$gcc_dir <- NULL
  pyDarwinOptions$rscript_path <- NULL
  expect_snapshot_value(pyDarwinOptions, style = "json2")
})

test_that("create_pyDarwinOptions GBRT", {
  pyDarwinOptions <-
    create_pyDarwinOptions(
      algorithm = "GBRT",
      nlme_dir = "~/InstallDirNLME/",
      rscript_path = "Rscript"
    )

  pyDarwinOptions$gcc_dir <- NULL
  pyDarwinOptions$rscript_path <- NULL
  expect_snapshot_value(pyDarwinOptions, style = "json2")
})

test_that("create_pyDarwinOptions RF", {
  pyDarwinOptions <-
    create_pyDarwinOptions(
      algorithm = "RF",
      nlme_dir = "~/InstallDirNLME/",
      rscript_path = "Rscript"
    )

  pyDarwinOptions$gcc_dir <- NULL
  pyDarwinOptions$rscript_path <- NULL
  expect_snapshot_value(pyDarwinOptions, style = "json2")
})

test_that("create_pyDarwinOptions GP", {
  pyDarwinOptions <-
    create_pyDarwinOptions(
      algorithm = "GP",
      nlme_dir = "~/InstallDirNLME/",
      rscript_path = "Rscript"
    )

  pyDarwinOptions$gcc_dir <- NULL
  pyDarwinOptions$rscript_path <- NULL
  expect_snapshot_value(pyDarwinOptions, style = "json2")
})

test_that("create_pyDarwinOptions MOGA NSGA-II default", {
  pyDarwinOptions <-
    create_pyDarwinOptions(
      algorithm = "MOGA",
      engine_adapter = "nonmem", # For effect_limit testing
      nmfe_path = "nmfe75",
      effect_limit = 5,
      rscript_path = "Rscript"
    )
  # local_2_bit_search should be NULL for MOGA
  expect_null(pyDarwinOptions$local_2_bit_search)

  pyDarwinOptions$gcc_dir <- NULL
  pyDarwinOptions$rscript_path <- NULL
  expect_snapshot_value(pyDarwinOptions, style = "json2")
})

test_that("create_pyDarwinOptions MOGA NSGA-III with postprocessing and constraints", {
  pyDarwinOptions <-
    create_pyDarwinOptions(
      algorithm = "MOGA3",
      MOGA = pyDarwinOptionsMOGA(objectives = 3, constraints = 1, partitions = 10),
      postprocess = pyDarwinOptionsPostprocess(use_r = TRUE, post_run_r_code = "test.R"),
      population_size = 60,
      rscript_path = "Rscript",
      nlme_dir = "~/InstallDirNLME/",
      engine_adapter = "nlme",
      effect_limit = 5
    )

  expect_null(pyDarwinOptions$effect_limit) # Should be NULL with nlme if strict interpretation
  pyDarwinOptions$gcc_dir <- NULL
  pyDarwinOptions$rscript_path <- NULL
  expect_snapshot_value(pyDarwinOptions, style = "json2")
})

test_that("create_pyDarwinOptions new general options and interactions", {
  pyDarwinOptions <- create_pyDarwinOptions(
    algorithm = "GA", # GA to allow downhill and effect_limit with nonmem
    engine_adapter = "nonmem",
    nmfe_path = "nmfe75", # Required for nonmem
    effect_limit = 6,
    local_grid_search = TRUE,
    max_local_grid_search_bits = 3,
    rerun_front_models = TRUE, # Note: rerun_front_models is usually for MOGA, but test it generally
    non_dominated_models_dir = "ndm_dir",
    keep_files = c("sim.ext"),
    keep_extensions = c("grd", "phi"),
    skip_running = TRUE,
    # Omega search for nonmem
    search_omega_bands = TRUE,
    individual_omega_search = TRUE,
    search_omega_sub_matrix = TRUE,
    max_omega_sub_matrix = 2,
    # Pass an extra undefined argument to test ...
    my_custom_darwin_param = "test_value",
    rscript_path = "Rscript"
  )

  pyDarwinOptions$gcc_dir <- NULL
  pyDarwinOptions$rscript_path <- NULL
  expect_snapshot_value(pyDarwinOptions, style = "json2")
})

test_that("create_pyDarwinOptions nlme omega search", {
  pyDarwinOptions_nlme_omega <- create_pyDarwinOptions(
    algorithm = "GA",
    nlme_dir = "~/InstallDirNLME/",
    engine_adapter = "nlme",
    search_omega_blocks = TRUE,
    individual_omega_search = FALSE,
    search_omega_sub_matrix = FALSE, # This should null max_omega_sub_matrix
    rscript_path = "Rscript"
  )
  expect_null(pyDarwinOptions_nlme_omega$search_omega_bands)
  expect_null(pyDarwinOptions_nlme_omega$nmfe_path)
  expect_null(pyDarwinOptions_nlme_omega$max_omega_sub_matrix)
  expect_equal(pyDarwinOptions_nlme_omega$search_omega_blocks, TRUE)
  expect_equal(pyDarwinOptions_nlme_omega$individual_omega_search, FALSE)


  pyDarwinOptions_nlme_omega$gcc_dir <- NULL # OS dependent
  pyDarwinOptions_nlme_omega$rscript_path <- NULL # Path normalization dependent
  expect_snapshot_value(pyDarwinOptions_nlme_omega, style = "json2")
})


# Test write_pyDarwinOptions()
test_that("write_pyDarwinOptions writes a valid JSON file with new options", {
  author <- "Jane Smith"
  project_name <- "MyNewProject"
  algorithm <- "MOGA3"
  MOGA_opts <- pyDarwinOptionsMOGA(objectives = 3, constraints = 1, partitions = 8)
  PSO_opts <- pyDarwinOptionsPSO(inertia = 0.6) # Keep a different algo option for completeness
  random_seed <- 123
  num_parallel <- 2
  num_generations <- 20
  population_size <- 30
  crash_value <- 1e8
  penalty_opts <- pyDarwinOptionsPenalty(covariance = 50)
  effect_limit_val <- 5
  local_grid_search_val <- TRUE
  max_local_grid_search_bits_val <- 2
  model_run_timeout_val <- 600
  postprocess_opts <- pyDarwinOptionsPostprocess(use_python = TRUE, post_run_python_code = "post.py")
  rerun_front_models_val <- FALSE
  non_dominated_models_dir_val <- "{working_dir}/front_runners"
  skip_running_val <- TRUE
  keep_files_val <- c("output.lst")
  keep_extensions_val <- c("ext", "cov")
  engine_adapter_val <- "nonmem"
  nmfe_path_val <- "/opt/nm75/nmfe75"
  rscript_path_val <- "rs_path" # Keep simple

  pyDarwinOptions <-
    create_pyDarwinOptions(
      author = author,
      project_name = project_name,
      algorithm = algorithm,
      MOGA = MOGA_opts,
      PSO = PSO_opts, # Will be NULLed due to algo=MOGA, testing this
      random_seed = random_seed,
      num_parallel = num_parallel,
      num_generations = num_generations,
      population_size = population_size,
      crash_value = crash_value,
      penalty = penalty_opts,
      effect_limit = effect_limit_val,
      local_grid_search = local_grid_search_val,
      max_local_grid_search_bits = max_local_grid_search_bits_val,
      model_run_timeout = model_run_timeout_val,
      postprocess = postprocess_opts,
      rerun_front_models = rerun_front_models_val,
      non_dominated_models_dir = non_dominated_models_dir_val,
      skip_running = skip_running_val,
      keep_files = keep_files_val,
      keep_extensions = keep_extensions_val,
      engine_adapter = engine_adapter_val,
      nmfe_path = nmfe_path_val,
      rscript_path = rscript_path_val,
      # explicit FALSE for a boolean
      remove_temp_dir = FALSE,
      search_omega_bands = FALSE # Test explicit false that gets NULLed if not applicable
    )

  # PSO should be NULL because algorithm is MOGA
  expect_null(pyDarwinOptions$PSO)
  # search_omega_bands is NULL because it's FALSE
  expect_null(pyDarwinOptions$search_omega_bands)
  # For snapshot consistency
  pyDarwinOptions$gcc_dir <- NULL
  pyDarwinOptions$rscript_path <- NULL


  tmp_file <- file.path(tempdir(), "temp_new_opts.json")
  write_pyDarwinOptions(pyDarwinOptions, file = tmp_file)

  expect_snapshot_file(tmp_file, compare = compare_file_text)
})
