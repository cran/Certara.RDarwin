# Test create_pyDarwinOptions()
test_that("create_pyDarwinOptions returns a valid list", {
  pyDarwinOptions <-
    create_pyDarwinOptions(
      author = "John Doe",
      algorithm = "GA",
      population_size = 10,
      rscript_path = "Rscript",
      postprocess = pyDarwinOptionsPostprocess()
    )

  # need to reset due to OS distinctions
  pyDarwinOptions$gcc_dir <- NULL
  expect_snapshot_value(pyDarwinOptions, style = "json2")
})

test_that("create_pyDarwinOptions EX", {
  pyDarwinOptions <-
    create_pyDarwinOptions(
      algorithm = "EX",
      rscript_path = "Rscript"
    )

  pyDarwinOptions$gcc_dir <- NULL
  expect_snapshot_value(pyDarwinOptions, style = "json2")
})

test_that("create_pyDarwinOptions GBRT", {
  pyDarwinOptions <-
    create_pyDarwinOptions(
      algorithm = "GBRT",
      rscript_path = "Rscript"
    )

  pyDarwinOptions$gcc_dir <- NULL
  expect_snapshot_value(pyDarwinOptions, style = "json2")
})

test_that("create_pyDarwinOptions RF", {
  pyDarwinOptions <-
    create_pyDarwinOptions(
      algorithm = "RF",
      rscript_path = "Rscript"
    )

  pyDarwinOptions$gcc_dir <- NULL
  expect_snapshot_value(pyDarwinOptions, style = "json2")
})

test_that("create_pyDarwinOptions GP", {
  pyDarwinOptions <-
    create_pyDarwinOptions(
      algorithm = "GP",
      rscript_path = "Rscript"
    )

  pyDarwinOptions$gcc_dir <- NULL
  expect_snapshot_value(pyDarwinOptions, style = "json2")
})

# Test write_pyDarwinOptions()
test_that("write_pyDarwinOptions writes a valid JSON file", {
  author <- "John Smith"
  project_name <- "MyProject"
  algorithm <- "PSO"
  GA <- pyDarwinOptionsGA(crossover_rate = 0.9)
  PSO <- pyDarwinOptionsPSO(inertia = 0.6, cognitive = 0.4)
  random_seed <- 42
  num_parallel <- 8
  num_generations <- 10
  population_size <- 20
  num_opt_chains <- 6
  exhaustive_batch_size <- 200
  crash_value <- 9999999
  penalty <-
    pyDarwinOptionsPenalty(theta = 3.84,
                           omega = 5,
                           sigma = 5)
  downhill_period <- 3
  num_niches <- 4
  niche_radius <- 3
  local_2_bit_search <- FALSE
  final_downhill_search <- FALSE
  model_run_timeout <- 1800
  model_run_priority_class <- "normal"
  rscript_path <- "/bin/Rscript"
  postprocess <-
    pyDarwinOptionsPostprocess(use_r = TRUE,
                               r_timeout = 60)
  use_saved_models <- TRUE
  saved_models_file <- "{working_dir}/models.json"
  saved_models_readonly <- TRUE
  remove_run_dir <- TRUE
  remove_temp_dir <- FALSE
  use_system_options <- FALSE
  model_cache <- "darwin.AsyncMemoryModelCache"
  model_run_man <- "darwin.GridRunManager"
  engine_adapter <- "nlme"
  working_dir <- "~/my_darwin_project/"
  data_dir <- "{project_dir}/data"
  output_dir <- "{project_dir}/output"
  temp_dir <- "{working_dir}/temp"
  nlme_dir <- "C:/Program Files/Certara/NLME_Engine"
  gcc_dir <- "C:/Program Files/Certara/mingw64"

  nlme_license <- "~/lservrc"
  # Create the pyDarwin options
  pyDarwinOptions <-
    create_pyDarwinOptions(
      author = author,
      project_name = project_name,
      algorithm = algorithm,
      PSO = PSO,
      random_seed = random_seed,
      num_parallel = num_parallel,
      num_generations = num_generations,
      population_size = population_size,
      num_opt_chains = num_opt_chains,
      exhaustive_batch_size = exhaustive_batch_size,
      crash_value = crash_value,
      penalty = penalty,
      downhill_period = downhill_period,
      num_niches = num_niches,
      niche_radius = niche_radius,
      local_2_bit_search = local_2_bit_search,
      final_downhill_search = final_downhill_search,
      model_run_timeout = model_run_timeout,
      model_run_priority_class = model_run_priority_class,
      postprocess = postprocess,
      use_saved_models = use_saved_models,
      saved_models_file = saved_models_file,
      saved_models_readonly = saved_models_readonly,
      remove_run_dir = remove_run_dir,
      remove_temp_dir = remove_temp_dir,
      use_system_options = use_system_options,
      model_cache = model_cache,
      model_run_man = model_run_man,
      engine_adapter = engine_adapter,
      working_dir = working_dir,
      data_dir = data_dir,
      output_dir = output_dir,
      temp_dir = temp_dir,
      nlme_dir = nlme_dir,
      gcc_dir = gcc_dir,
      rscript_path = rscript_path,
      nlme_license = nlme_license
    )

  tmp_file <- file.path(tempdir(), "temp.json")
  # Write the options to the temporary file
  write_pyDarwinOptions(pyDarwinOptions, file = tmp_file)

  # Check if the JSON data matches the original options
  expect_snapshot_file(tmp_file, compare = compare_file_text)
})
