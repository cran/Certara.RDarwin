#' Create pyDarwin Options
#'
#' Generates a list of parameters to be used in a pyDarwin run.
#'
#' @param author Character string: The name of the author.
#' @param project_name Character string (optional): The name of the project. If
#'   not specified, pyDarwin will set its value to the name of the parent folder
#'   of the options file.
#' @param algorithm Character string: One of EX, GA, GP, RF, GBRT, PSO.
#'   See section Details below for more information.
#' @param GA List: Options specific to the Genetic Algorithm (GA). See
#'   [pyDarwinOptionsGA()].
#' @param PSO List: Options specific to the Particle Swarm Optimization (PSO).
#'   See [pyDarwinOptionsPSO()].
#' @param random_seed Positive integer: Seed for random number generation.
#' @param num_parallel Positive integer: Number of models to execute in
#'   parallel, i.e., how many threads to create to handle model runs.
#' @param num_generations Positive integer: Number of iterations or generations
#'   of the search algorithm to run. Not used/required for EX.
#' @param population_size Positive integer: Number of models to create in every
#'   generation. Not used/required for EX.
#' @param num_opt_chains Positive integer: Number of parallel processes to
#'   perform the "ask" step (to increase performance). Required only for GP, RF,
#'   and GBRT.
#' @param exhaustive_batch_size Positive integer: Batch size for the EX
#'   (Exhaustive Search) algorithm.
#' @param crash_value Positive real: Value of fitness or reward assigned when
#'   model output is not generated. Should be set larger than any anticipated
#'   completed model fitness.
#' @param penalty List: Options specific to the penalty calculation. See
#'   [pyDarwinOptionsPenalty()].
#' @param downhill_period Integer: How often to run the downhill step. If < 1,
#'   no periodic downhill search will be performed.
#' @param num_niches Integer: Used for GA and downhill. A penalty is assigned
#'   for each model based on the number of similar models within a niche radius.
#'   This penalty is applied only to the selection process (not to the fitness
#'   of the model). The purpose is to ensure maintaining a degree of diversity
#'   in the population (integer). `num_niches` is also used to select the number
#'   of models that are entered into the downhill step for all algorithms,
#'   except EX.
#' @param niche_radius Positive real: The radius of the niches. The niche radius
#'   is used to define how similar pairs of models are. This is used to select
#'   models for the Local search, as requested, and to calculate the sharing
#'   penalty for Genetic Algorithm.
#' @param local_2_bit_search Logical: Whether to perform the two-bit local
#'   search. The two-bit local search substantially increases the robustness of
#'   the search. All downhill local searches are done starting from num_niches
#'   models.
#' @param final_downhill_search Logical: Whether to perform a local search
#'   (1-bit and 2-bit) at the end of the global search.
#' @param search_omega_blocks Logical: whether to perform search for block
#'   omegas. Used only when `engine_adapter == 'nlme'`.
#' @param search_omega_bands Logical: whether to perform search for band
#'   omegas. Used only when `engine_adapter == 'nonmem'`.
#' @param individual_omega_search  Logical: If set, every search block will be
#'   handled individually: each block will have a separate gene and max omega
#'   search length (either calculated or set explicitly in the options). If set
#'   to `FALSE`, all search blocks will have the same pattern of block omegas.
#'   Default is `TRUE`.
#' @param search_omega_sub_matrix Logical: set to true to search omega
#'   submatrix. Default is `FALSE`.
#' @param max_omega_sub_matrix Integer: Maximum size of sub matrix to use in
#'  search. Default is 4.
#' @param model_run_timeout Positive real: Time (seconds) after which the
#'   execution will be terminated, and the crash value assigned.
#' @param model_run_priority_class Character string (Windows only): Priority
#'   class for child processes that build and run models, as well as run the R
#'   postprocess script. Options are `below_normal` and `normal`. `below_normal`
#'   is recommended to maintain user interface responsiveness.
#' @param postprocess List: Options specific to postprocessing. See
#'   [pyDarwinOptionsPostprocess()]
#' @param keep_key_models Logical: Key model is the best model in population
#'   (generation). Such models may be a subject of interest when the search is
#'   analyzed, so they should be saved separately with all their output. Default
#'   is `TRUE`
#' @param use_saved_models Logical: Whether to restore saved Model Cache from
#'   file. Default is `FALSE`.
#' @param saved_models_file Character string: The file from which to restore
#'   Model Cache. Will only have an effect if use_saved_models is set to true.
#'   By default, the cache is saved in \{working_dir\}/models.json and cleared
#'   every time the search is started. To use saved runs, rename models.json or
#'   copy it to a different location.
#' @param saved_models_readonly Logical: Do not overwrite the saved_models_file
#'   content. Default is `FALSE`.
#' @param remove_run_dir Logical: If TRUE, will delete the entire model run
#'   directory, otherwise - only unnecessary files inside it. Default is
#'   `FALSE`.
#' @param remove_temp_dir Logical: Whether to delete the entire temp_dir after
#'   the search is finished or stopped. Doesn't have any effect when the search
#'   is run on a grid. Default is `TRUE`.
#' @param use_system_options Logical: Whether to override options with
#'   environment-specific values. Default is `TRUE`.
#' @param model_cache Character string: ModelCache subclass to be used.
#'   Currently, there are only `darwin.MemoryModelCache` and
#'   `darwin.AsyncMemoryModelCache`. You can create your own and use it (e.g., a
#'   cache that stores model runs in a database). The name is quite arbitrary
#'   and doesn't have any convention/constraints.
#' @param model_run_man Character string: ModelRunManager subclass to be used.
#'   Currently, there are only `darwin.LocalRunManager` and
#'   `darwin.GridRunManager`.
#' @param engine_adapter Character string: ModelEngineAdapter subclass to be
#'   used. Currently only `nlme` (default) and `nonmem` are available.
#' @param working_dir Character string: The project's working directory, where
#'   all the necessary files and folders are created. By default, it is set to
#'   `<pyDarwin home>/{project_stem}`, where `{project_stem}` is a file system
#'   friendly representation of the project name in a way that it will be easy
#'   to manage as a folder name where all non-letters and non-digits are
#'   replaced with underscores.
#' @param data_dir Character string: Directory where datasets are located. Must
#'   be available for individual model runs. Default in pyDarwin if not given:
#'   `{project_dir}`.
#' @param output_dir Character string: Directory where pyDarwin output will be
#'   placed. Default is `{working_dir}/output`.
#' @param temp_dir Character string: Parent directory for all model runs' run
#'   directories, i.e., where all folders for every iteration are located.
#'   Default in pyDarwin if not given: `{working_dir}/temp`.
#' @param nlme_dir Character string: Directory where the NLME Engine is
#'   installed/unzipped. Default: `C:/Program Files/Certara/NLME_Engine`.
#'   Used only when `engine_adapter == 'nlme'`.
#' @param gcc_dir Character string: Directory where the Mingw-w64 compiler (gcc)
#'   is installed. Default: `C:/Program Files/Certara/mingw64` for Windows and
#'   gcc version found by `which gcc` on Linux.
#'   Used only when `engine_adapter == 'nlme'`.
#' @param nmfe_path Character string: Directory where NONMEM is installed.
#' Used only when `engine_adapter == 'nonmem'`.
#' @param rscript_path Character string: Path to the Rscript executable. By
#'   default, it is obtained with R.home("bin").
#' @param nlme_license Character string (optional): Path to the license file. If
#'   not provided, pyDarwin will set its value to PhoenixLicenseFile (only for
#'   current Python session).
#' @param generic_grid_adapter List: Options specific to the grids. See
#'   [pyDarwinOptionsGridAdapter()]
#' @param ... Additional parameters: Other arguments not explicitly defined in
#'   the function's signature are allowed and will be stored in the options
#'   list. See
#'   \href{https://certara.github.io/pyDarwin/html/Options.html}{pyDarwin
#'   documentation}.
#'
#' @return A list of pyDarwin options.
#'
#' @details The algorithm parameter specifies the type of search algorithm to be
#'   used in the pyDarwin optimization process. It determines the strategy and
#'   approach used to explore the search space and find the optimal solution.
#'   The following are the available options for the algorithm parameter.
#'
#'   "EX" (Exhaustive Search Algorithm): The exhaustive search algorithm is a
#'   simple and straightforward method to explore the entire search space
#'   systematically. The search space is initially represented as a string of
#'   integers, one for each dimension. The algorithm exhaustively evaluates all
#'   candidate models within the search space, making it best suited for small
#'   search spaces with a limited number of dimensions. Due to its exhaustive
#'   nature, it is not practical for large search spaces with millions of
#'   possible models.
#'
#'   "GA" (Genetic Algorithm): The genetic algorithm is an evolutionary
#'   optimization technique inspired by natural selection and genetics. It
#'   employs techniques such as selection, crossover, and mutation to evolve a
#'   population of candidate models over multiple generations. By applying
#'   natural selection principles, the genetic algorithm aims to converge
#'   towards better-performing models. It is suitable for moderate to large
#'   search spaces and can handle a diverse range of problem types.
#'
#'   "GP" (Gaussian Process Algorithm): The Gaussian Process is one of the two
#'   options used in Bayesian Optimization. It specifies the form of the prior
#'   and posterior distribution for model evaluations. Initially, the
#'   distribution is random, similar to other global search algorithms. As
#'   models are executed and their results are obtained, the distribution is
#'   updated using the "ask" and "tell" steps. The Gaussian Process aims to use
#'   probabilistic models to guide the search towards promising regions of the
#'   search space efficiently. It is particularly useful for
#'   expensive-to-evaluate functions and can handle both continuous and discrete
#'   variables.
#'
#'   "RF" (Random Forest Algorithm):  The Random Forest algorithm is an ensemble
#'   learning method that constructs multiple decision trees during the
#'   optimization process. It leverages bagging and random feature selection to
#'   increase the precision of tree building. By combining multiple trees, the
#'   Random Forest aims to achieve higher accuracy and robustness in the
#'   optimization process. It is effective for a wide range of problem types and
#'   can handle both regression and classification tasks.
#'
#'   "GBRT" (Gradient Boosted Random Tree Algorithm): The Gradient Boosted
#'   Random Tree algorithm is a variation of the Random Forest approach. It
#'   builds trees progressively by calculating the gradient of the reward or
#'   fitness with respect to each decision. This allows the algorithm to focus
#'   on challenging regions of the search space, which can lead to increased
#'   precision and improved optimization results. Similar to Random Forest, it
#'   is suitable for regression and classification problems.
#'
#'   "PSO" (Particle Swarm Optimization Algorithm): The Particle Swarm
#'   Optimization algorithm is a population-based optimization technique
#'   inspired by the social behavior of bird flocks or fish schools. It
#'   represents potential solutions as particles that move through the search
#'   space to find the optimal solution. Particles communicate and share
#'   information about their current best-known positions, allowing them to
#'   explore promising areas collaboratively. The Particle Swarm Optimization is
#'   effective for continuous optimization problems and can handle noisy or
#'   multimodal objective functions.
#'
#'   When using the create_pyDarwinOptions function, you can specify one of
#'   these algorithm values to choose the appropriate optimization strategy for
#'   your specific problem. Each algorithm has its strengths and limitations,
#'   and the choice of algorithm should be based on the problem's
#'   characteristics and the desired search space exploration behavior.
#'
#'   Please see
#'   \href{https://certara.github.io/pyDarwin/html/Options.html}{pyDarwin
#'   documentation} for more details.#'
#'
#' @examples
#' # Create pyDarwin options with default values
#' pyDarwinOptions <- create_pyDarwinOptions()
#' # Create pyDarwin options with custom author and algorithm
#' pyDarwinOptions <-
#'   create_pyDarwinOptions(author = "John Doe",
#'                          algorithm = "PSO")
#'
#' @export
create_pyDarwinOptions <- function(author = "",
                                   project_name = NULL,
                                   algorithm = c("GA", "EX", "GP", "RF", "GBRT", "PSO"),
                                   GA = pyDarwinOptionsGA(),
                                   PSO = pyDarwinOptionsPSO(),
                                   random_seed = 11,
                                   num_parallel = 4,
                                   num_generations = 6,
                                   population_size = 4,
                                   num_opt_chains = 4,
                                   exhaustive_batch_size = 100,
                                   crash_value = 99999999,
                                   penalty = pyDarwinOptionsPenalty(),
                                   downhill_period = 2,
                                   num_niches = 2,
                                   niche_radius = 2,
                                   local_2_bit_search = TRUE,
                                   final_downhill_search = TRUE,
                                   search_omega_blocks = FALSE,
                                   search_omega_bands = FALSE,
                                   individual_omega_search = TRUE,
                                   search_omega_sub_matrix = FALSE,
                                   max_omega_sub_matrix = 4,
                                   model_run_timeout = 1200,
                                   model_run_priority_class = c("below_normal", "normal"),
                                   postprocess = pyDarwinOptionsPostprocess(),
                                   keep_key_models = TRUE,
                                   use_saved_models = FALSE,
                                   saved_models_file = "{working_dir}/models0.json",
                                   saved_models_readonly = FALSE,
                                   remove_run_dir = FALSE,
                                   remove_temp_dir = TRUE,
                                   use_system_options = TRUE,
                                   model_cache = "darwin.MemoryModelCache",
                                   model_run_man = c("darwin.LocalRunManager",
                                                     "darwin.GridRunManager"),
                                   engine_adapter = c("nlme", "nonmem"),
                                   working_dir = NULL,
                                   data_dir = NULL,
                                   output_dir = "{working_dir}/output",
                                   temp_dir = NULL,
                                   nlme_dir = "C:/Program Files/Certara/NLME_Engine",
                                   gcc_dir = "C:/Program Files/Certara/mingw64",
                                   nmfe_path = NULL,
                                   rscript_path = file.path(R.home("bin"), "Rscript"),
                                   nlme_license = NULL,
                                   generic_grid_adapter = pyDarwinOptionsGridAdapter(),
                                   ...) {
  stopifnot(is.character(author))
  algorithm <- match.arg(algorithm)
  stopifnot(is.list(GA))
  stopifnot(is.list(PSO))
  stopifnot(is.numeric(num_parallel))
  stopifnot(is.numeric(num_generations))
  stopifnot(is.numeric(population_size))
  stopifnot(is.numeric(num_opt_chains))
  stopifnot(is.numeric(exhaustive_batch_size))
  stopifnot(is.numeric(crash_value))
  stopifnot(is.list(penalty))
  stopifnot(is.numeric(downhill_period))
  stopifnot(is.numeric(num_niches))
  stopifnot(is.numeric(niche_radius))
  stopifnot(is.logical(local_2_bit_search))
  stopifnot(is.logical(final_downhill_search))
  stopifnot(is.logical(individual_omega_search))
  stopifnot(is.logical(search_omega_sub_matrix))
  stopifnot(is.numeric(max_omega_sub_matrix))
  stopifnot(is.numeric(model_run_timeout))
  model_run_priority_class <- match.arg(model_run_priority_class)
  stopifnot(is.list(postprocess))
  stopifnot(is.logical(keep_key_models))
  stopifnot(is.logical(use_saved_models))
  stopifnot(is.character(saved_models_file))
  stopifnot(is.logical(saved_models_readonly))
  stopifnot(is.logical(remove_run_dir))
  stopifnot(is.logical(remove_temp_dir))
  stopifnot(is.logical(use_system_options))
  stopifnot(is.character(model_cache))
  model_run_man <- match.arg(model_run_man)
  engine_adapter = match.arg(engine_adapter)
  stopifnot(is.character(nlme_dir))
  stopifnot(is.character(gcc_dir))
  stopifnot(is.character(rscript_path))
  stopifnot(is.list(generic_grid_adapter))

  if (algorithm == "EX") {
    GA <- NULL
    PSO <- NULL
    num_generations <- NULL
    population_size <- NULL
    num_opt_chains <- NULL
    downhill_period <- NULL
    num_niches <- NULL
    niche_radius <- NULL
    local_2_bit_search <- NULL
    final_downhill_search <- NULL
    if (engine_adapter == "nlme") {
      random_seed <- NULL
    } else {
      # NONMEM
      if (missing(search_omega_bands) ||
          is.null(search_omega_bands) ||
          !search_omega_bands) {
        random_seed <- NULL
      }
    }
  } else {
    exhaustive_batch_size <- NULL

    if (algorithm != "PSO") {
      PSO <- NULL
    }

    if (algorithm != "GA") {
      GA <- NULL
    }

    if (!algorithm %in% c("GP", "RF", "GBRT")) {
      num_opt_chains <- NULL
    }
  }

  if (algorithm != "GA" &&
      !is.null(downhill_period) &&
      downhill_period < 1 &&
      !is.null(final_downhill_search) &&
      !final_downhill_search) {
    num_niches <- NULL
    niche_radius <- NULL
  }

  if (is.null(use_saved_models) ||
      !use_saved_models) {
    saved_models_file <- NULL
  }

  if (is.null(postprocess$use_r) &&
      is.null(postprocess$use_python)) {
    postprocess <- NULL
  }

  if (engine_adapter == "nlme") {
    nmfe_path <- NULL
    if (!missing(search_omega_bands)) {
      warning("'search_omega_bands' argument is not used ",
              "when engine_adapter == 'nlme'")
    }

    search_omega_bands <- NULL

    if (missing(search_omega_blocks) ||
        is.null(search_omega_blocks) ||
        !search_omega_blocks) {
      search_omega_blocks <- NULL
      individual_omega_search <- NULL
      search_omega_sub_matrix <- NULL
      max_omega_sub_matrix <- NULL
    } else {
      # omega_blocks to be searched
      if (!search_omega_sub_matrix) {
        max_omega_sub_matrix <- NULL
      }
    }

    if (missing(gcc_dir) &&
        model_run_man == "darwin.LocalRunManager" &&
        .Platform$OS.type == "unix") {
      GCCLocation <- system("which gcc", intern = TRUE)
      if (length(GCCLocation) == 1 &&
          file.exists(GCCLocation)) {
        gcc_dir <- dirname(dirname(GCCLocation))
      } else {
        warning("gcc_dir argument is not given and it was not found \n",
                "on the system. Please specify it before run.")
      }
    }
  } else {
    # NONMEM
    nlme_dir <- NULL
    gcc_dir <- NULL
    nlme_license <- NULL

    if (!missing(search_omega_blocks)) {
      warning("'search_omega_blocks' argument is not used ",
              "when engine_adapter == 'nonmem'")
    }

    search_omega_blocks <- NULL

    if (missing(search_omega_bands) ||
        is.null(search_omega_bands) ||
        !search_omega_bands) {
      search_omega_bands <- NULL
      individual_omega_search <- NULL
      search_omega_sub_matrix <- NULL
      max_omega_sub_matrix <- NULL
    } else {
      # omega bands to be searched
      if (!search_omega_sub_matrix) {
        max_omega_sub_matrix <- NULL
      }
    }
  }

  if (model_run_man != "darwin.GridRunManager") {
    generic_grid_adapter <- NULL
  }

  if (missing(rscript_path)) {
    if (.Platform$OS.type == "windows") {
      # need to add .exe for normalization
      rscript_path <- paste0(rscript_path, ".exe")
    }

    rscript_path <- normalizePath(rscript_path, mustWork = NA)
  }

  ReturnedList <- list(
    author = author,
    project_name = project_name,
    algorithm = algorithm,
    GA = GA,
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
    search_omega_blocks = search_omega_blocks,
    search_omega_bands = search_omega_bands,
    individual_omega_search = individual_omega_search,
    search_omega_sub_matrix = search_omega_sub_matrix,
    max_omega_sub_matrix = max_omega_sub_matrix,
    model_run_timeout = model_run_timeout,
    model_run_priority_class = model_run_priority_class,
    postprocess = postprocess,
    keep_key_models = keep_key_models,
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
    nlme_license = nlme_license,
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
