#' Create pyDarwin Options
#'
#' Generates a list of parameters to be used in a pyDarwin run.
#'
#' @param author Character string: The name of the author.
#' @param project_name Character string (optional): The name of the project. If
#'   not specified, pyDarwin will set its value to the name of the parent folder
#'   of the options file.
#' @param algorithm Character string: One of EX, GA, MOGA, MOGA3, GP, RF, GBRT,
#'   PSO. See section Details below for more information.
#' @param GA List: Options specific to the Genetic Algorithm (GA).
#'   See [pyDarwinOptionsGA()]. Ignored if algorithm is not "GA".
#' @param MOGA List: Options specific to the Multi-Objective Genetic Algorithm
#'   (MOGA or MOGA3). See [pyDarwinOptionsMOGA()]. Ignored if algorithm is not
#'   "MOGA" or "MOGA3".
#' @param PSO List: Options specific to the Particle Swarm Optimization (PSO).
#'   See [pyDarwinOptionsPSO()]. Ignored if algorithm is not "PSO".
#' @param random_seed Positive integer: Seed for random number generation.
#' @param num_parallel Positive integer: Number of models to execute in
#'   parallel, i.e., how many threads to create to handle model runs.
#'   Default: 4.
#' @param num_generations Positive integer: Number of iterations or generations
#'   of the search algorithm to run. Not used/required for EX. Default: 6.
#' @param population_size Positive integer: Number of models to create in every
#'   generation. Not used/required for EX. Default: 4.
#' @param num_opt_chains Positive integer: Number of parallel processes to
#'   perform the "ask" step (to increase performance). Required only for GP, RF,
#'   and GBRT. Default: 4.
#' @param exhaustive_batch_size Positive integer: Batch size for the EX
#'   (Exhaustive Search) algorithm. Default: 100.
#' @param crash_value Positive real: Value of fitness or reward assigned when
#'   model output is not generated. Should be set larger than any anticipated
#'   completed model fitness. Default: 99999999.
#' @param penalty List: Options specific to the penalty calculation.
#'   See [pyDarwinOptionsPenalty()].
#' @param effect_limit Integer: Limits number of effects. Applicable only for
#'   NONMEM and GA/MOGA/MOGA3. If < 1, effect limit is turned off. Default: -1.
#' @param downhill_period Integer: How often to run the downhill step. If < 1,
#'   no periodic downhill search will be performed. Default: 2.
#' @param num_niches Integer: Used for GA and downhill. A penalty is assigned
#'   for each model based on the number of similar models within a niche radius.
#'   This penalty is applied only to the selection process (not to the fitness
#'   of the model). The purpose is to ensure maintaining a degree of diversity
#'   in the population. `num_niches` is also used to select the number of models
#'   that are entered into the downhill step for all algorithms, except EX.
#'   Default: 2.
#' @param niche_radius Positive real: The radius of the niches. Used to define
#'   how similar pairs of models are, for Local search and GA sharing penalty.
#'   Default: 2.
#' @param local_2_bit_search Logical: Whether to perform the two-bit local
#'   search. Substantially increases search robustness. Done starting from
#'   `num_niches` models. Ignored for MOGA and MOGA3. Default: TRUE.
#' @param final_downhill_search Logical: Whether to perform a local search
#'   (1-bit and 2-bit) at the end of the global search. Default: TRUE.
#' @param local_grid_search Logical: Whether to perform a local grid search
#'   during downhill. Default: FALSE.
#' @param max_local_grid_search_bits Positive integer: Maximum number of bits
#'   to explore in the local grid search. Default: 5.
#' @param search_omega_blocks Logical: Whether to perform search for block
#'   omegas. Used only when `engine_adapter == 'nlme'`. Default: FALSE.
#' @param search_omega_bands Logical: Whether to perform search for band
#'   omegas. Used only when `engine_adapter == 'nonmem'`. Default: FALSE.
#' @param individual_omega_search Logical: If TRUE, every omega search block is
#'   handled individually. If FALSE, all search blocks have the same pattern.
#'   Default: TRUE.
#' @param search_omega_sub_matrix Logical: Set to TRUE to search omega
#'   submatrix. Default: FALSE.
#' @param max_omega_sub_matrix Integer: Maximum size of sub matrix to use in
#'   search. Default: 4.
#' @param model_run_timeout Positive real: Time (seconds) after which the
#'   execution will be terminated, and the crash value assigned. Default: 1200.
#' @param model_run_priority_class Character string (Windows only): Priority
#'   class for child processes. Options are `below_normal` (default) and
#'   `normal`.
#' @param postprocess List: Options specific to postprocessing.
#'   See [pyDarwinOptionsPostprocess()]. For `algorithm = "MOGA3"`,
#'   postprocessing is required to define objectives and constraints. For
#'   `algorithm = "MOGA"` (NSGA-II), pyDarwin does not use postprocessing for
#'   objective calculation.
#' @param keep_key_models Logical: Whether to save the best model from every
#'   generation to `key_models_dir`. Default: TRUE.
#' @param keep_best_models Logical: If `TRUE` (default), saves only "key"
#'   models that represent an improvement in fitness value compared to the
#'   previous overall best model. Models are saved to `key_models_dir`.
#'   Not applicable to Exhaustive Search (EX). Default: `TRUE`.
#' @param rerun_key_models Logical: Whether to re-run key models that lack
#'   output after the search. Default: FALSE.
#' @param rerun_front_models Logical: Similar to `rerun_key_models`, but for
#'   non-dominated models (typically from MOGA/MOGA3). Models are copied to
#'   `non_dominated_models_dir`. Default: TRUE.
#' @param use_saved_models Logical: Whether to restore saved Model Cache from
#'   file. Default: FALSE.
#' @param saved_models_file Character string: The file from which to restore
#'   Model Cache. Default: "\{working_dir\}/models0.json".
#' @param saved_models_readonly Logical: Do not overwrite the
#'   `saved_models_file` content. Default: FALSE.
#' @param remove_run_dir Logical: If TRUE, delete the entire model run
#'   directory, otherwise only unnecessary files. Default: FALSE.
#' @param remove_temp_dir Logical: Whether to delete the entire `temp_dir`
#'   after the search. Default: FALSE
#' @param keep_files Character vector (optional): List of exact file names to
#'   keep when cleaning up run directories.
#'   Default is `c("dmp.txt", "posthoc.csv")` when `engine_adapter` is "nlme".
#' @param keep_extensions Character vector (optional): List of file extensions
#'   (without dot) to keep. Default: NULL.
#' @param use_system_options Logical: Whether to override options with
#'   environment-specific values. Default: TRUE.
#' @param model_cache Character string: ModelCache subclass to be used.
#'   Default: "darwin.MemoryModelCache".
#' @param model_run_man Character string: ModelRunManager subclass to be used.
#'   Options: "darwin.LocalRunManager" (default), "darwin.GridRunManager".
#' @param engine_adapter Character string: ModelEngineAdapter subclass.
#'   Options: "nlme" (default), "nonmem".
#' @param skip_running Logical: If TRUE, no actual NM/NLME runs will be
#'   performed. Default: FALSE.
#' @param working_dir Character string (optional): Project's working directory.
#' @param data_dir Character string (optional): Directory for datasets.
#' @param output_dir Character string: Directory for pyDarwin output.
#'   Default: "\{working_dir\}/output".
#' @param temp_dir Character string (optional): Parent directory for model run
#'   subdirectories.
#' @param key_models_dir Character string: Directory where key/best models
#'   will be saved. Default: "\{working_dir\}/key_models".
#' @param non_dominated_models_dir Character string: Directory where
#'   non-dominated models will be saved (typically for MOGA/MOGA3).
#'   Default: "\{working_dir\}/non_dominated_models".
#' @param nlme_dir Character string (optional): Directory for NLME Engine
#'   installation.
#' @param gcc_dir Character string (optional): Directory for Mingw-w64 compiler.
#' @param nmfe_path Character string (optional): Path to NONMEM execution
#'   command.
#' @param rscript_path Character string (optional): Path to Rscript executable.
#' @param generic_grid_adapter List: Options for grid execution.
#'   See [pyDarwinOptionsGridAdapter()]. Used if
#'   `model_run_man == "darwin.GridRunManager"`.
#' @param remote_run Logical: Indicates if pyDarwin execution is for a remote
#'   host. Default: `FALSE`.
#' @param ... Additional parameters.
#'
#' @return A list of pyDarwin options.
#'
#' @details The algorithm parameter specifies the search algorithm. The
#'   algorithm “MOGA” and “MOGA3” are used for multi-objective optimization:
#'   "MOGA" uses NSGA-II (see the documentation at
#'   \url{https://pymoo.org/algorithms/moo/nsga2.html?highlight=nsga%20ii}),
#'   and "MOGA3" uses NSGA-III (see the documentation at
#'   \url{https://pymoo.org/algorithms/moo/nsga3.html?highlight=nsga%20ii}).
#'   For MOGA3, the objectives and constraints must be defined and returned by
#'   postprocessing scripts (post_run_r_code or post_run_python_code) in a
#'   specific format:
#'   - R scripts should return a list of two vectors: the first vector is for
#'     the objectives and the second one is for the constraints. If no
#'     constraints, the second vector should be empty.
#'   - Python scripts should return a tuple of two lists: the first list is for
#'     the objectives and the second one is for the constraints). If no
#'     constraints, the second list should be empty.
#'
#'   Other algorithms include "EX" (Exhaustive), "GA" (Genetic Algorithm), "GP"
#'   (Gaussian Process), "RF" (Random Forest), "GBRT" (Gradient Boosted Random
#'   Tree), and "PSO" (Particle Swarm Optimization).
#'
#'   Please see
#'   \href{https://certara.github.io/pyDarwin/html/Options.html}{pyDarwin documentation}
#'   for complete details on all options.
#'
#' @examples
#' # Basic options with GA
#' ga_opts <- create_pyDarwinOptions(author = "Jane Doe", algorithm = "GA")
#'
#' # Options for MOGA (NSGA-II)
#' # pyDarwin internally uses 2 objectives; postprocessing for objectives is not used by pyDarwin.
#' moga_opts_nsga2 <- create_pyDarwinOptions(
#'   author = "J. Doe",
#'   project_name = "MOGA_Test_NSGA2",
#'   algorithm = "MOGA", # NSGA-II
#'   MOGA = pyDarwinOptionsMOGA(), # Default MOGA options are suitable
#'   population_size = 50,
#'   num_generations = 100,
#'   engine_adapter = "nonmem",
#'   nmfe_path = "/opt/NONMEM/nm75/run/nmfe75"
#' )
#'
#' # Options for MOGA3 (NSGA-III with 3 objectives, 1 constraint via R postprocessing)
#' moga_opts_nsga3_custom <- pyDarwinOptionsMOGA(
#'   objectives = 3,
#'   names = c("AIC", "NumEffects", "RunTime"), # Example custom names
#'   constraints = 1,
#'   partitions = 10 # Custom partitions
#' )
#' main_opts_nsga3 <- create_pyDarwinOptions(
#'   author = "J. Doe",
#'   project_name = "MOGA_Test_NSGA3",
#'   algorithm = "MOGA3", # NSGA-III
#'   MOGA = moga_opts_nsga3_custom,
#'   population_size = 60, # NSGA-III population size might need adjustment
#'   num_generations = 100,
#'   postprocess = pyDarwinOptionsPostprocess( # Required for MOGA3
#'     use_r = TRUE,
#'     post_run_r_code = "{project_dir}/moga3_postprocess.R"
#'   ),
#'   engine_adapter = "nonmem",
#'   nmfe_path = "/opt/NONMEM/nm75/run/nmfe75"
#' )
#'
#' @export
create_pyDarwinOptions <- function(author = "",
                                   project_name = NULL,
                                   algorithm = c("GA", "EX", "MOGA", "MOGA3", "GP", "RF", "GBRT", "PSO"),
                                   GA = pyDarwinOptionsGA(),
                                   MOGA = pyDarwinOptionsMOGA(),
                                   PSO = pyDarwinOptionsPSO(),
                                   random_seed = 11,
                                   num_parallel = 4,
                                   num_generations = 6,
                                   population_size = 4,
                                   num_opt_chains = 4,
                                   exhaustive_batch_size = 100,
                                   crash_value = 99999999,
                                   penalty = pyDarwinOptionsPenalty(),
                                   effect_limit = -1,
                                   downhill_period = 2,
                                   num_niches = 2,
                                   niche_radius = 2,
                                   local_2_bit_search = TRUE,
                                   final_downhill_search = TRUE,
                                   local_grid_search = FALSE,
                                   max_local_grid_search_bits = 5,
                                   search_omega_blocks = FALSE,
                                   search_omega_bands = FALSE,
                                   individual_omega_search = TRUE,
                                   search_omega_sub_matrix = FALSE,
                                   max_omega_sub_matrix = 4,
                                   model_run_timeout = 1200,
                                   model_run_priority_class = c("below_normal", "normal"),
                                   postprocess = pyDarwinOptionsPostprocess(),
                                   keep_key_models = TRUE,
                                   keep_best_models = TRUE,
                                   rerun_key_models = FALSE,
                                   rerun_front_models = TRUE,
                                   use_saved_models = FALSE,
                                   saved_models_file = "{working_dir}/models0.json",
                                   saved_models_readonly = FALSE,
                                   remove_run_dir = FALSE,
                                   remove_temp_dir = FALSE,
                                   keep_files = c("dmp.txt", "posthoc.csv"),
                                   keep_extensions = NULL,
                                   use_system_options = TRUE,
                                   model_cache = "darwin.MemoryModelCache",
                                   model_run_man = c("darwin.LocalRunManager", "darwin.GridRunManager"),
                                   engine_adapter = c("nlme", "nonmem"),
                                   skip_running = FALSE,
                                   working_dir = NULL,
                                   data_dir = NULL,
                                   output_dir = "{working_dir}/output",
                                   temp_dir = NULL,
                                   key_models_dir = "{working_dir}/key_models",
                                   non_dominated_models_dir = "{working_dir}/non_dominated_models",
                                   nlme_dir = "C:/Program Files/Certara/NLME_Engine",
                                   gcc_dir = "C:/Program Files/Certara/mingw64",
                                   nmfe_path = NULL,
                                   rscript_path = file.path(normalizePath(R.home("bin")), "Rscript"),
                                   generic_grid_adapter = pyDarwinOptionsGridAdapter(),
                                   remote_run = FALSE,
                                   ...) {
  stopifnot(is.character(author))
  if (!is.null(project_name))
    stopifnot(is.character(project_name))
  algorithm <- match.arg(algorithm)
  stopifnot(is.list(GA))
  stopifnot(is.list(MOGA))
  stopifnot(is.list(PSO))
  if (!is.null(random_seed))
    stopifnot(is.numeric(random_seed))
  stopifnot(is.numeric(num_parallel), num_parallel > 0)
  if (!is.null(num_generations))
    stopifnot(is.numeric(num_generations), num_generations > 0)
  if (!is.null(population_size))
    stopifnot(is.numeric(population_size), population_size > 0)
  if (!is.null(num_opt_chains))
    stopifnot(is.numeric(num_opt_chains), num_opt_chains > 0)
  if (!is.null(exhaustive_batch_size))
    stopifnot(is.numeric(exhaustive_batch_size),
              exhaustive_batch_size > 0)
  stopifnot(is.numeric(crash_value))
  stopifnot(is.list(penalty))
  stopifnot(is.numeric(effect_limit))
  if (!is.null(downhill_period))
    stopifnot(is.numeric(downhill_period))
  if (!is.null(num_niches))
    stopifnot(is.numeric(num_niches))
  if (!is.null(niche_radius))
    stopifnot(is.numeric(niche_radius), niche_radius > 0)
  if (!is.null(local_2_bit_search))
    stopifnot(is.logical(local_2_bit_search))
  if (!is.null(final_downhill_search))
    stopifnot(is.logical(final_downhill_search))
  stopifnot(is.logical(local_grid_search))
  stopifnot(is.numeric(max_local_grid_search_bits),
            max_local_grid_search_bits > 0)
  if (!is.null(search_omega_blocks))
    stopifnot(is.logical(search_omega_blocks))
  if (!is.null(search_omega_bands))
    stopifnot(is.logical(search_omega_bands))
  if (!is.null(individual_omega_search))
    stopifnot(is.logical(individual_omega_search))
  if (!is.null(search_omega_sub_matrix))
    stopifnot(is.logical(search_omega_sub_matrix))
  if (!is.null(max_omega_sub_matrix))
    stopifnot(is.numeric(max_omega_sub_matrix))
  stopifnot(is.numeric(model_run_timeout), model_run_timeout > 0)
  model_run_priority_class <- match.arg(model_run_priority_class)
  if (!is.null(postprocess))
    stopifnot(is.list(postprocess))
  stopifnot(is.logical(keep_key_models))
  stopifnot(is.logical(keep_best_models))
  stopifnot(is.logical(rerun_key_models))
  stopifnot(is.logical(rerun_front_models))
  stopifnot(is.logical(use_saved_models))
  if (!is.null(saved_models_file))
    stopifnot(is.character(saved_models_file))
  stopifnot(is.logical(saved_models_readonly))
  stopifnot(is.logical(remove_run_dir))
  stopifnot(is.logical(remove_temp_dir))
  if (!is.null(keep_files))
    stopifnot(is.character(keep_files))
  if (!is.null(keep_extensions))
    stopifnot(is.character(keep_extensions))
  stopifnot(is.logical(use_system_options))
  stopifnot(is.character(model_cache))
  model_run_man <- match.arg(model_run_man)
  engine_adapter <- match.arg(engine_adapter)
  stopifnot(is.logical(skip_running))
  if (!is.null(working_dir))
    stopifnot(is.character(working_dir))
  if (!is.null(data_dir))
    stopifnot(is.character(data_dir))
  stopifnot(is.character(output_dir))
  if (!is.null(temp_dir))
    stopifnot(is.character(temp_dir))
  stopifnot(is.character(key_models_dir))
  stopifnot(is.character(non_dominated_models_dir))
  if (!is.null(nlme_dir))
    stopifnot(is.character(nlme_dir))
  if (!is.null(gcc_dir))
    stopifnot(is.character(gcc_dir))
  if (!is.null(nmfe_path))
    stopifnot(is.character(nmfe_path))
  if (!is.null(generic_grid_adapter))
    stopifnot(is.list(generic_grid_adapter))
  stopifnot(is.logical(remote_run))

  # Conditional logic based on algorithm and other settings
  if (algorithm == "EX") {
    GA <- NULL
    MOGA <- NULL
    PSO <- NULL
    num_generations <- NULL
    population_size <- NULL
    num_opt_chains <- NULL
    downhill_period <- NULL
    num_niches <- NULL
    niche_radius <- NULL
    local_2_bit_search <- NULL
    final_downhill_search <- NULL
    local_grid_search <- NULL
    max_local_grid_search_bits <- NULL
    rerun_key_models <- NULL
    rerun_front_models <- NULL
    # keep_best_models is also not applicable to EX according to pyD docs
    keep_best_models <- NULL
  } else {
    exhaustive_batch_size <- NULL

    if (algorithm != "GA") {
      GA <- NULL
    }

    # Nullify MOGA block if algorithm is not MOGA or MOGA3
    if (!algorithm %in% c("MOGA", "MOGA3")) {
      MOGA <- NULL
    }

    if (algorithm != "PSO") {
      PSO <- NULL
    }

    if (!algorithm %in% c("GP", "RF", "GBRT")) {
      num_opt_chains <- NULL
    }

    # ignore options not suitable for MOGA and MOGA3
    if (algorithm %in% c("MOGA", "MOGA3")) {
      local_2_bit_search <- NULL
      penalty <- NULL
      keep_key_models <- NULL
      keep_best_models <- NULL
      rerun_key_models <- NULL
      key_models_dir <- NULL
    }
  }

  # effect_limit applicable only for NONMEM and GA/MOGA/MOGA3
  if (!(engine_adapter == "nonmem" &&
        algorithm %in% c("GA", "MOGA", "MOGA3"))) {
    effect_limit <- NULL
  }

  # num_niches and niche_radius logic for GA and downhill
  if (algorithm != "GA") {
    if ((is.null(downhill_period) || downhill_period < 1) &&
        (is.null(final_downhill_search) ||
         !final_downhill_search) &&
        (is.null(local_grid_search) || !local_grid_search)) {
      num_niches <- NULL
      niche_radius <- NULL
    }
  }

  # If local_grid_search is false, max_local_grid_search_bits is irrelevant
  if (!is.null(local_grid_search) && !local_grid_search) {
    max_local_grid_search_bits <- NULL
  }

  # saved_models_file and saved_models_readonly logic
  if (is.null(use_saved_models) || !use_saved_models) {
    saved_models_file <- NULL
    if (!is.null(saved_models_readonly))
      saved_models_readonly <- NULL
  }

  # Postprocess logic
  # pyDarwin docs: "Postprocessing is not used for MOGA."
  # For MOGA3, postprocessing is required.
  if (!is.null(postprocess)) {
    is_postprocess_configured <-
      (
        !is.null(postprocess$use_r) &&
          postprocess$use_r && !is.null(postprocess$post_run_r_code)
      ) ||
      (
        !is.null(postprocess$use_python) &&
          postprocess$use_python &&
          !is.null(postprocess$post_run_python_code)
      )

    if (length(postprocess) == 0 || !is_postprocess_configured) {
      postprocess <- NULL
    }

    if (algorithm == "MOGA" && !is.null(postprocess)) {
      warning(
        "Algorithm is 'MOGA' (NSGA-II); postprocessing options are provided",
        " but will be ignored by pyDarwin for objective calculation.",
        call. = FALSE
      )
      # Do not nullify postprocess here, user might have other uses for it,
      # pyDarwin ignores it for objectives.
    } else if (algorithm == "MOGA3" && is.null(postprocess)) {
      warning(
        "Algorithm is 'MOGA3' (NSGA-III), but postprocessing is not configured. ",
        "Postprocessing is required for MOGA3 to define objectives and constraints.",
        call. = FALSE
      )
    }
  }


  # engine_adapter specific logic (nlme vs nonmem)
  if (engine_adapter == "nlme") {
    nmfe_path <- NULL
    if (!missing(search_omega_bands) &&
        !is.null(search_omega_bands) && search_omega_bands) {
      warning("'search_omega_bands' argument is not used when engine_adapter == 'nlme'",
              call. = FALSE)
    }
    search_omega_bands <- NULL

    if (missing(search_omega_blocks) ||
        is.null(search_omega_blocks) || !search_omega_blocks) {
      search_omega_blocks <- NULL
      individual_omega_search <- NULL
      search_omega_sub_matrix <- NULL
      max_omega_sub_matrix <- NULL
    } else {
      search_omega_blocks <- TRUE
      if (!is.null(search_omega_sub_matrix) &&
          !search_omega_sub_matrix) {
        max_omega_sub_matrix <- NULL
        search_omega_sub_matrix <- NULL
      } else if (!is.null(search_omega_sub_matrix) &&
                 search_omega_sub_matrix) {
        search_omega_sub_matrix <- TRUE
      }
    }

    if (missing(gcc_dir) &&
        model_run_man == "darwin.LocalRunManager" &&
        .Platform$OS.type == "unix" && !remote_run) {
      GCCLocation <-
        tryCatch(
          system(
            "which gcc",
            intern = TRUE,
            ignore.stderr = TRUE
          ),
          warning = function(w)
            ""
        )
      if (length(GCCLocation) == 1 &&
          nzchar(GCCLocation[1]) && file.exists(GCCLocation[1])) {
        gcc_dir <- dirname(dirname(GCCLocation[1]))
      } else {
        warning(
          "gcc_dir argument is not given and 'which gcc' failed to find it on this Unix system.",
          call. = FALSE
        )
      }
    }

    if (.Platform$OS.type == "unix" || remote_run) {
      if (missing(nlme_dir)) {
        warning(
          "nlme_dir is not specified but engine_adapter is 'nlme'. ",
          "This will likely cause an error in pyDarwin unless overridden by system options.",
          call. = FALSE
        )
      }
      if (missing(gcc_dir)) {
        warning(
          "gcc_dir is not specified but engine_adapter is 'nlme'. ",
          "This will likely cause an error in pyDarwin unless overridden by system options.",
          call. = FALSE
        )
      }
    }
  } else {
    # NONMEM
    if (remote_run) {
      warning("Remote runs with `NONMEM` engine_adapter are not supported.",
              call. = FALSE)
    }
    nlme_dir <- NULL
    gcc_dir <- NULL

    if (!missing(search_omega_blocks) &&
        !is.null(search_omega_blocks) && search_omega_blocks) {
      warning(
        "'search_omega_blocks' argument is not used when engine_adapter == 'nonmem'",
        call. = FALSE
      )
    }
    search_omega_blocks <- NULL

    if (missing(search_omega_bands) ||
        is.null(search_omega_bands) || !search_omega_bands) {
      search_omega_bands <- NULL
      individual_omega_search <- NULL
      search_omega_sub_matrix <- NULL
      max_omega_sub_matrix <- NULL
    } else {
      search_omega_bands <- TRUE
      if (!is.null(search_omega_sub_matrix) &&
          !search_omega_sub_matrix) {
        max_omega_sub_matrix <- NULL
        search_omega_sub_matrix <- NULL
      } else if (!is.null(search_omega_sub_matrix) &&
                 search_omega_sub_matrix) {
        search_omega_sub_matrix <- TRUE
      }
    }

    if (is.null(nmfe_path))
      warning(
        "nmfe_path is not specified but engine_adapter is 'nonmem'. ",
        "This will likely cause an error in pyDarwin unless overridden by system options.",
        call. = FALSE
      )
  }

  if (model_run_man != "darwin.GridRunManager") {
    generic_grid_adapter <- NULL
  }

  # rscript_path logic (largely unchanged)
  # Check if rscript_path is needed (for postprocessing or NLME)
  r_is_used_in_postprocess <-
    !is.null(postprocess) &&
    !is.null(postprocess$use_r) &&
    postprocess$use_r && !is.null(postprocess$post_run_r_code)
  is_rscript_needed <-
    r_is_used_in_postprocess ||
    (engine_adapter == "nlme" &&
       !skip_running) # NLME might need Rscript

  if (is_rscript_needed &&
      (is.null(rscript_path) || !nzchar(rscript_path))) {
    # If R is used in postprocessing or NLME is the engine, and rscript_path is not set,
    # try to default it, but warn if it might be an issue for remote runs.
    if (remote_run) {
      warning(
        "rscript_path is not specified for a remote run where R scripts may be used (postprocessing or NLME). This may cause errors unless set in system options on the remote machine.",
        call. = FALSE
      )
    } else if (is.null(rscript_path)) {
      # only set default if it was truly NULL
      rscript_path <-
        file.path(normalizePath(R.home("bin")), "Rscript")
      if (.Platform$OS.type == "windows" &&
          !grepl("\\.exe$", rscript_path, ignore.case = TRUE)) {
        rscript_path <- paste0(rscript_path, ".exe")
      }
      if (!file.exists(rscript_path)) {
        warning(
          paste0(
            "Default rscript_path '",
            rscript_path,
            "' does not exist. Specify rscript_path if R scripts are used."
          ),
          call. = FALSE
        )
        rscript_path <- NULL # Set to NULL if default doesn't exist
      }
    }
  } else if (!is.null(rscript_path) &&
             .Platform$OS.type == "windows" && !remote_run) {
    rscript_path <- normalizePath(rscript_path, mustWork = FALSE)
    if (!grepl("\\.exe$", rscript_path, ignore.case = TRUE)) {
      rscript_path <- paste0(rscript_path, ".exe")
    }
  } else if (is.null(rscript_path) || !nzchar(rscript_path)) {
    rscript_path <-
      NULL # Ensure it's NULL if not needed or not provided
  }


  # keep_key_models and rerun_key_models logic (unchanged)
  if (!is.null(rerun_key_models) &&
      rerun_key_models &&
      (is.null(keep_key_models) || !keep_key_models)) {
    warning(
      "rerun_key_models is TRUE, but keep_key_models is FALSE. ",
      "Key models must be kept to be rerun. Setting keep_key_models to TRUE.",
      call. = FALSE
    )
    keep_key_models <- TRUE
  }

  # keep_best_models implies keep_key_models (from pyD docs)
  if (!is.null(keep_best_models) &&
      keep_best_models &&
      (is.null(keep_key_models) || !keep_key_models)) {
    # This case implies if keep_best_models is true, keep_key_models should also be true.
    # The original code for rerun_key_models already handles setting keep_key_models to TRUE.
    # We can add a direct set here if keep_best_models is TRUE.
    keep_key_models <- TRUE
  }


  if (engine_adapter == "nonmem") {
    default_files <- c("dmp.txt", "posthoc.csv")
    matched_files <- default_files[default_files %in% keep_files]

    if (length(matched_files) > 0 && !missing(keep_files)) {
      message(
        paste(matched_files, collapse = ", "),
        " file(s) are the default output of NLME run. Please check if it is intended."
      )
    }

    # If keep_files is missing (default used) AND it contains only default files -> reset to NULL
    if (missing(keep_files) && all(default_files %in% keep_files)) {
      keep_files <- NULL
    }
  }

  # Normalize empty vectors to NULL
  if (length(keep_files) == 0) {
    keep_files <- NULL
  }
  if (length(keep_extensions) == 0) {
    keep_extensions <- NULL
  }

  ReturnedList <- list(
    author = author,
    project_name = project_name,
    algorithm = algorithm,
    GA = GA,
    MOGA = MOGA,
    PSO = PSO,
    random_seed = random_seed,
    num_parallel = num_parallel,
    num_generations = num_generations,
    population_size = population_size,
    num_opt_chains = num_opt_chains,
    exhaustive_batch_size = exhaustive_batch_size,
    crash_value = crash_value,
    penalty = penalty,
    effect_limit = effect_limit,
    downhill_period = downhill_period,
    num_niches = num_niches,
    niche_radius = niche_radius,
    local_2_bit_search = local_2_bit_search,
    final_downhill_search = final_downhill_search,
    local_grid_search = local_grid_search,
    max_local_grid_search_bits = max_local_grid_search_bits,
    search_omega_blocks = search_omega_blocks,
    search_omega_bands = search_omega_bands,
    individual_omega_search = individual_omega_search,
    search_omega_sub_matrix = search_omega_sub_matrix,
    max_omega_sub_matrix = max_omega_sub_matrix,
    model_run_timeout = model_run_timeout,
    model_run_priority_class = model_run_priority_class,
    postprocess = postprocess,
    keep_key_models = keep_key_models,
    keep_best_models = keep_best_models,
    rerun_key_models = rerun_key_models,
    rerun_front_models = rerun_front_models,
    use_saved_models = use_saved_models,
    saved_models_file = saved_models_file,
    saved_models_readonly = saved_models_readonly,
    remove_run_dir = remove_run_dir,
    remove_temp_dir = remove_temp_dir,
    keep_files = keep_files,
    keep_extensions = keep_extensions,
    use_system_options = use_system_options,
    model_cache = model_cache,
    model_run_man = model_run_man,
    engine_adapter = engine_adapter,
    skip_running = skip_running,
    working_dir = working_dir,
    data_dir = data_dir,
    output_dir = output_dir,
    temp_dir = temp_dir,
    key_models_dir = key_models_dir,
    non_dominated_models_dir = non_dominated_models_dir,
    nlme_dir = nlme_dir,
    gcc_dir = gcc_dir,
    nmfe_path = nmfe_path,
    rscript_path = rscript_path,
    generic_grid_adapter = generic_grid_adapter,
    ...
  )

  ReturnedList <- ReturnedList[!sapply(ReturnedList, is.null)]
  ReturnedList
}

.add_ArgumentToList <- function(List, Argument) {
  if (methods::hasArg(Argument)) {
    stopifnot(is.character(Argument))
    ReturnedList[[deparse(substitute(Argument))]] <- Argument
  }
  ReturnedList
}

#' Write pyDarwin options to a JSON file.
#'
#' This function takes a list of pyDarwin options and writes them to a JSON file
#' in the specified format. The options can be generated using the
#' `create_pyDarwinOptions` function or customized manually. The resulting JSON
#' file can be used as input for a pyDarwin model search.
#'
#' @param pyDarwinOptions A list containing the pyDarwin options to be written
#'   to the JSON file. Default is the result of calling [create_pyDarwinOptions()]
#'   with default arguments.
#' @param file Character: The path to the JSON file where the options will be
#'   written. Default is a file named "options.json" in the current working
#'   directory.
#' @inheritParams jsonlite::toJSON
#'
#' @return None (invisible `NULL`).
#'
#' @examples
#' # Write pyDarwin options to a JSON file
#' Options <-
#'   create_pyDarwinOptions(author = "John Doe",
#'                          algorithm = "GA",
#'                          population_size = 10)
#' write_pyDarwinOptions(Options,
#'                       file = file.path(tempdir(), "options.json"))
#'
#' @export
write_pyDarwinOptions <-
  function(pyDarwinOptions = create_pyDarwinOptions(),
           file = "options.json",
           pretty = TRUE,
           digits = NA,
           auto_unbox = TRUE) {
    if (is.numeric(pretty)) {
      indent <- pretty
    } else {
      indent <- 4
    }

    prettyJSON <-
      jsonlite::prettify(
        jsonlite::toJSON(
          as.list(pyDarwinOptions),
          pretty = pretty,
          digits = digits,
          auto_unbox = auto_unbox
        ),
        indent = indent
      )

    cat(prettyJSON,
        file = file, sep = "\n")
  }
