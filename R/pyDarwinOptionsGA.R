#' Create options for the Genetic Algorithm (GA) in pyDarwin.
#'
#' This function allows you to set various options specific to the Genetic
#' Algorithm (GA) in pyDarwin.
#'
#' @param elitist_num A positive integer specifying the number of best models
#'   from any generation to carry over, unchanged, to the next generation.
#'   Functions like the Hall of Fame in DEAP. Default: 2
#' @param crossover_rate A real value (between 0.0 and 1.0) specifying the
#'   fraction of mating pairs that will undergo crossover. Default: 0.95
#' @param mutation_rate A real value (between 0.0 and 1.0) specifying the
#'   probability that at least one bit in the genome will be “flipped”, 0 to 1,
#'   or 1 to 0. Default: 0.95
#' @param sharing_alpha A real value specifying the parameter of the niche
#'   penalty calculation. Default: 0.1
#' @param selection A string specifying the selection algorithm for the GA.
#'   Currently, only "tournament" is available. Default: "tournament"
#' @param selection_size A positive integer specifying the number of “parents”
#'   to enter in the selection. 2 is highly recommended, experience with other
#'   values is very limited. Default: 2
#' @param crossover_operator A string specifying the algorithm for crossover.
#'   Only "cxOnePoint" (single-point crossover) is available. Default:
#'   "cxOnePoint"
#' @param mutate A string specifying the algorithm for mutation. Currently, only
#'   "flipBit" is available. Default: "flipBit"
#' @param attribute_mutation_probability A real value specifying the probability
#'   of any bit being mutated (real value between 0.0 and 1.0). Default: 0.1
#' @param niche_penalty A positive real value used for the calculation of the
#'   crowding penalty. The niche penalty is calculated by first finding the
#'   “distance matrix”, the pair-wise Mikowski distance from the present model
#'   to all other models. The “crowding” quantity is then calculated as the sum
#'   of: (distance/niche_radius)^sharing_alpha for all other models in the
#'   generation for which the Mikowski distance is less than the niche radius.
#'   Finally, the penalty is calculated as: exp((crowding-1)*niche_penalty)-1.
#'   The objective of using a niche penalty is to maintain diversity of models,
#'   to avoid premature convergence of the search by penalizing when models are
#'   too similar to other models in the current generation. Default: 20
#'
#' @return An object of class "pyDarwinOptionsGA" containing the specified GA
#'   options.
#'
#' @examples
#'
#' # Create GA options with default values
#' options <- pyDarwinOptionsGA()
#'
#' # Create GA options with custom values
#' options <-
#'   pyDarwinOptionsGA(elitist_num = 4,
#'                     crossover_rate = 0.9,
#'                     mutation_rate = 0.8,
#'                     sharing_alpha = 0.2)
#'
#' @export
pyDarwinOptionsGA <- function(elitist_num = 2,
                              crossover_rate = 0.95,
                              mutation_rate = 0.95,
                              sharing_alpha = 0.1,
                              selection = "tournament",
                              selection_size = 2,
                              crossover_operator = "cxOnePoint",
                              mutate = "flipBit",
                              attribute_mutation_probability = 0.1,
                              niche_penalty = 20) {
  stopifnot(is.numeric(elitist_num))
  stopifnot(is.numeric(crossover_rate))
  stopifnot(is.numeric(mutation_rate))
  stopifnot(is.numeric(sharing_alpha))
  selection <- match.arg(selection)
  stopifnot(is.numeric(selection_size))
  crossover_operator <- match.arg(crossover_operator)
  mutate <- match.arg(mutate)
  stopifnot(is.numeric(attribute_mutation_probability))
  stopifnot(is.numeric(niche_penalty))

  list(
    elitist_num = elitist_num,
    crossover_rate = crossover_rate,
    mutation_rate = mutation_rate,
    sharing_alpha = sharing_alpha,
    selection = selection,
    selection_size = selection_size,
    crossover_operator = crossover_operator,
    mutate = mutate,
    attribute_mutation_probability = attribute_mutation_probability,
    niche_penalty = niche_penalty
  )
}
