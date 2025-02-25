#' Create pyDarwin Penalty Options
#'
#' Generates a list of penalty parameters to be used in pyDarwin
#' create_pyDarwinOptions function.
#'
#' @param theta numeric: Penalty added to fitness/reward for each
#'   estimated THETA. A value of 3.84 corresponds to a hypothesis test with 1 df
#'   and p < 0.05 (for nested models), and a value of 2 for 1 df corresponds to
#'   the Akaike information criterion. Default: 10
#' @param omega numeric: Penalty added to fitness/reward for each
#'   estimated OMEGA element. Default: 10
#' @param sigma numeric: Penalty added to fitness/reward for each
#'   estimated SIGMA element. Default: 10
#' @param convergence numeric: Penalty added to fitness/reward for
#'   failing to converge. Default: 100
#' @param covariance numeric: Penalty added to fitness/reward for
#'   failing the covariance step (real number). If a successful covariance step
#'   is important, this can be set to a large value (e.g., 100), otherwise, set
#'   to 0. Default: 100
#' @param correlation numeric: Penalty added to fitness/reward if any
#'   off-diagonal element of the correlation matrix of estimates has an absolute
#'   value > 0.95 (real number). This penalty will be added if the covariance
#'   step fails or is not requested. Default: 100
#' @param condition_number numeric: Penalty added if the covariance
#'   step fails or is not requested, e.g., PRINT=E is not included in $COV.
#'   Additionally, if the covariance is successful and the condition number of
#'   the covariance matrix is > 1000, then this penalty is added to the
#'   fitness/reward. Default: 100
#' @param non_influential_tokens numeric: Penalty added to
#'   fitness/reward if any tokens do not influence the control file (relevant
#'   for nested tokens). Should be very small (e.g., 0.0001), as the purpose is
#'   only for the model with non-influential tokens to be slightly worse than
#'   the same model without the non-influential token(s) to break a tie.
#'   Default: 0.00001
#'
#' @return A list of penalty options in pyDarwin optimization process.
#'
#' @examples
#' # Create penalty options with default values
#' penalty_options <- pyDarwinOptionsPenalty()
#' # Create penalty options with custom values
#' penalty_options_custom <-
#'   pyDarwinOptionsPenalty(theta = 3.84,
#'                          omega = 8,
#'                          sigma = 6,
#'                          convergence = 50,
#'                          covariance = 80,
#'                          correlation = 60,
#'                          condition_number = 70,
#'                          non_influential_tokens = 0.0001)
#'
#' @export
pyDarwinOptionsPenalty <- function(theta = 10,
                                   omega = 10,
                                   sigma = 10,
                                   convergence = 100,
                                   covariance = 100,
                                   correlation = 100,
                                   condition_number = 100,
                                   non_influential_tokens = 0.00001) {
  stopifnot(is.numeric(theta))
  stopifnot(is.numeric(omega))
  stopifnot(is.numeric(sigma))
  stopifnot(is.numeric(convergence))
  stopifnot(is.numeric(covariance))
  stopifnot(is.numeric(correlation))
  stopifnot(is.numeric(condition_number))
  stopifnot(is.numeric(non_influential_tokens))

  list(
    theta = theta,
    omega = omega,
    sigma = sigma,
    convergence = convergence,
    covariance = covariance,
    correlation = correlation,
    condition_number = condition_number,
    non_influential_tokens = non_influential_tokens
  )
}
