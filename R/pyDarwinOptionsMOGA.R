#' Create Options for the pyDarwin MOGA Block
#'
#' Generates a list of specific options for the MOGA (Multi-Objective Genetic
#' Algorithm) or MOGA3 (NSGA-III) algorithms in pyDarwin. This list is
#' intended to be passed as the `MOGA` argument to the
#' `create_pyDarwinOptions()` function when `algorithm` is set to "MOGA" or
#' "MOGA3".
#'
#' @param objectives Positive integer: Number of objectives.
#'   Applicable only when the `algorithm` in `create_pyDarwinOptions()`
#'   is set to "MOGA3". If the `algorithm` is "MOGA" (implying NSGA-II),
#'   this parameter is ignored by pyDarwin, and 2 objectives (OFV and NEP)
#'   are used internally. For "MOGA3", objectives are defined by
#'   postprocessing.
#'   Default: 3 (relevant for "MOGA3").
#' @param names Character vector (optional): List of names for the objectives.
#'   Applicable only when the `algorithm` is "MOGA3". The length of
#'   this vector should match the `objectives` value. If `NULL`, empty, or of
#'   a different size, pyDarwin uses generic names (e.g., "f1", "f2", "f3").
#'   These names are used for reporting in `results.csv`. Ignored if the
#'   `algorithm` is "MOGA".
#'   Default: NULL.
#' @param constraints Non-negative integer: Number of constraints.
#'   Applicable only when the `algorithm` is "MOGA3". Constraints must be
#'   provided by user-defined postprocessing scripts (R or Python).
#'   See \url{https://pymoo.org/constraints/index.html}. Ignored if the
#'   `algorithm` is "MOGA".
#'   Default: 0.
#' @param partitions Positive integer: Number of partitions for the reference
#'   directions used in NSGA-III.
#'   Applicable only when the `algorithm` is "MOGA3".
#'   See \url{https://pymoo.org/misc/reference_directions.html}. Ignored if the
#'   `algorithm` is "MOGA".
#'   Default: 12.
#' @param crossover Character string: Crossover algorithm.
#'   When set to "single", SinglePointCrossover is used. Otherwise, for other
#'   values like "two_point", TwoPointCrossover is typically used by pymoo.
#'   See \url{https://pymoo.org/operators/crossover.html#Point-Crossover}.
#'   Applicable for both "MOGA" and "MOGA3".
#'   Default: "single".
#' @param crossover_rate Numeric value between 0.0 and 1.0: The fraction of
#'   mating pairs that will undergo crossover. Applicable for both "MOGA" and
#'   "MOGA3".
#'   Default: 0.95.
#' @param mutation_rate Numeric value between 0.0 and 1.0: The probability
#'   that at least one bit in the genome will be "flipped" (mutated).
#'   Applicable for both "MOGA" and "MOGA3".
#'   Default: 0.95.
#' @param attribute_mutation_probability Numeric value between 0.0 and 1.0:
#'   The probability of any individual bit (attribute) in the genome being
#'   mutated. Applicable for both "MOGA" and "MOGA3".
#'   Default: 0.1.
#'
#' @return A list containing MOGA-specific options.
#'
#' @details
#' This function defines parameters for the `MOGA` settings block in pyDarwin.
#' The relevance of certain parameters (`objectives`, `names`, `constraints`,
#' `partitions`) depends on whether the `algorithm` in
#' `create_pyDarwinOptions()` is set to "MOGA" (for NSGA-II) or "MOGA3"
#' (for NSGA-III).
#'
#' - If `algorithm = "MOGA"` (NSGA-II):
#'   pyDarwin internally uses 2 objectives (OFV and NEP). The `objectives`,
#'   `names`, `constraints`, and `partitions` parameters from this MOGA options
#'   block are ignored by pyDarwin. Postprocessing scripts are not used by
#'   pyDarwin to define objectives.
#'
#' - If `algorithm = "MOGA3"` (NSGA-III):
#'   The `objectives`, `names`, `constraints`, and `partitions` parameters
#'   from this MOGA options block are utilized by pyDarwin. User-supplied
#'   postprocessing scripts (R or Python) are **required** to calculate and
#'   return the values for the objectives and constraints.
#'   (R: list of two vectors; Python: tuple of two lists).
#'
#' Common parameters like `crossover`, `crossover_rate`, `mutation_rate`, and
#' `attribute_mutation_probability` apply to both "MOGA" and "MOGA3" variants.
#'
#' @examples
#' # MOGA options, defaults are generally suitable for algorithm = "MOGA3"
#' # if postprocessing handles 3 objectives.
#' moga_block_for_moga3 <- pyDarwinOptionsMOGA(
#'   objectives = 3,
#'   names = c("Objective1", "Objective2", "Objective3"),
#'   constraints = 1,
#'   partitions = 10
#' )
#'
#' # MOGA options where specific settings are for NSGA-II (algorithm = "MOGA")
#' # Note: objectives, names, constraints, partitions would be ignored by pyDarwin.
#' moga_block_for_moga <- pyDarwinOptionsMOGA(
#'   crossover = "two_point",
#'   crossover_rate = 0.92
#' )
#'
#' @export
pyDarwinOptionsMOGA <- function(objectives = 3,
                                names = NULL,
                                constraints = 0,
                                partitions = 12,
                                crossover = "single",
                                crossover_rate = 0.95,
                                mutation_rate = 0.95,
                                attribute_mutation_probability = 0.1) {
  stopifnot(is.numeric(objectives), objectives > 0)

  if (!is.null(names)) {
    stopifnot(is.character(names))
  }

  stopifnot(is.numeric(constraints), constraints >= 0)

  stopifnot(is.numeric(partitions), partitions > 0)

  stopifnot(is.character(crossover))

  stopifnot(is.numeric(crossover_rate),
            crossover_rate >= 0,
            crossover_rate <= 1.0)
  stopifnot(is.numeric(mutation_rate),
            mutation_rate >= 0,
            mutation_rate <= 1.0)
  stopifnot(
    is.numeric(attribute_mutation_probability),
    attribute_mutation_probability >= 0,
    attribute_mutation_probability <= 1.0
  )

  moga_opts_list <- list(
    objectives = objectives,
    constraints = constraints,
    partitions = partitions,
    crossover = crossover,
    crossover_rate = crossover_rate,
    mutation_rate = mutation_rate,
    attribute_mutation_probability = attribute_mutation_probability
  )

  if (!is.null(names)) {
    moga_opts_list$names <- names
  }

  moga_opts_list
}
