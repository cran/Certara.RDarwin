#' Create options for the Particle Swarm Optimization (PSO) in pyDarwin.
#'
#' This function allows you to set various options specific to the Particle
#' Swarm Optimization (PSO) in pyDarwin.
#'
#' @param inertia A real value specifying the particle coordination movement as
#'   it relates to the previous velocity (commonly denoted as w). Default: 0.4
#' @param cognitive A real value specifying the particle coordination movement
#'   as it relates to its own best known position (commonly denoted as c1).
#'   Default: 0.5
#' @param social A real value specifying the particle coordination movement as
#'   it relates to the current best known position across all particles
#'   (commonly denoted as c2). Default: 0.5
#' @param neighbor_num A positive integer specifying the number of neighbors
#'   that any particle interacts with to determine the social component of the
#'   velocity of the next step. A smaller number of neighbors results in a more
#'   thorough search (as the neighborhoods tend to move more independently,
#'   allowing the swarm to cover a larger section of the total search space) but
#'   will converge more slowly. Default: 20
#' @param p_norm A positive integer specifying the Minkowski p-norm to use. A
#'   value of 1 is the sum-of-absolute values (or L1 distance) while 2 is the
#'   Euclidean (or L2) distance. Default: 2
#' @param break_on_no_change A positive integer specifying the number of
#'   iterations used to determine whether the optimization has converged.
#'   Default: 5
#'
#' @return An object containing the specified options for the Particle Swarm
#'   Optimization (PSO) algorithm.
#'
#' @examples
#'
#' # Create PSO options with default values
#' options <- pyDarwinOptionsPSO()
#'
#' # Create PSO options with custom values
#' options <- pyDarwinOptionsPSO(inertia = 0.2,
#'                               cognitive = 0.8,
#'                               social = 0.7,
#'                               neighbor_num = 10)
#'
#' @export
pyDarwinOptionsPSO <- function(inertia = 0.4,
                               cognitive = 0.5,
                               social = 0.5,
                               neighbor_num = 20,
                               p_norm = 2,
                               break_on_no_change = 5) {
  stopifnot(is.numeric(inertia))
  stopifnot(is.numeric(cognitive))
  stopifnot(is.numeric(social))
  stopifnot(is.numeric(neighbor_num))
  stopifnot(is.numeric(p_norm))

  list(
    inertia = inertia,
    cognitive = cognitive,
    social = social,
    neighbor_num = neighbor_num,
    p_norm = p_norm,
    break_on_no_change = break_on_no_change
  )
}
