#' Create an InitialEstimate object
#'
#' @param Lower A numeric vector of lower bounds for the parameter estimate
#' @param Estimate A numeric vector of initial estimates for the parameter
#' @param Upper A numeric vector of upper bounds for the parameter estimate
#'
#' @return An object of class InitialEstimate
#'
#' @noRd
#' @keywords internal NLME
new_InitialEstimate <- function(Lower = numeric(),
                                Estimate = numeric(),
                                Upper = numeric()) {
  stopifnot(length(Estimate) != 0)
  stopifnot(is.numeric(Estimate) & all(!is.na(Estimate)))
  stopifnot(all(is.na(Lower)) | is.numeric(Lower))
  stopifnot(all(is.na(Upper)) | is.numeric(Upper))

  structure(list(
    Lower = Lower,
    Estimate = Estimate,
    Upper = Upper
  ),
  class = "InitialEstimate")
}

#' Validate an InitialEstimate object
#'
#' @param InitialEstimateInstance An object of class InitialEstimate
#'
#' @return The validated InitialEstimateInstance
#' @importFrom stats na.omit
#' @noRd
#' @keywords internal NLME
validate_InitialEstimate <- function(InitialEstimateInstance) {
  Lower <- InitialEstimateInstance$Lower
  Upper <- InitialEstimateInstance$Upper
  Estimate <- InitialEstimateInstance$Estimate

  if (length(Lower) != length(Estimate)) {
    stop("'Lower' length (", length(Lower),
         ") should be the same length as 'Estimate' (", length(Estimate), ")")
  }

  if (length(Estimate) != length(Upper)) {
    stop("'Upper' length (", length(Lower),
         ") should be the same length as 'Estimate' (", length(Estimate), ")")
  }

  LowerGreater <- Lower >= Estimate

  if (!all(is.na(LowerGreater)) &
      any(na.omit(LowerGreater))) {
    stop(paste(
      "Lower bound:",
      na.omit(Lower[LowerGreater]),
      "should be less than Estimate:",
      na.omit(Estimate[LowerGreater]),
      "\n"
    ))
  }

  UpperLower <- Upper <= Estimate
  if (!all(is.na(UpperLower)) &
      any(na.omit(UpperLower))) {
    stop(paste(
      "Upper bound:",
      na.omit(Upper[UpperLower]),
      "should be greater than Estimate:",
      na.omit(Estimate[UpperLower]),
      "\n"
    ))
  }

  InitialEstimateInstance
}

#' Extracts the initial estimate from a numeric vector.
#'
#' @param Initial A numeric vector containing either a single initial estimate or three elements representing
#' the lower bound, estimate, and upper bound.
#'
#' @return A list containing the extracted lower bound, estimate, and upper bound.
#'
#' @noRd
#' @keywords internal NLME
.extract_EstimateFromInitial <- function(Initial) {
  if (length(Initial) == 0) {
    stop("No initial estimates provided")
  } else if (length(Initial) == 1) {
    Estimate <- Initial
    Lower <- NA
    Upper <- NA
  } else if (length(Initial) == 3) {
    Estimate <- Initial[2]
    Lower <- Initial[1]
    Upper <- Initial[3]
  } else {
    stop("Initial estimates should have a length 1 or 3")
  }

  list(Lower = Lower,
       Estimate = Estimate,
       Upper = Upper)
}

#' Create an object of class InitialEstimate
#'
#' This function creates an object of class `InitialEstimate` that contains
#' initial parameter estimates for a model [Theta()]. The estimates can be
#' passed to the function as a single numeric value or as a vector of length
#' three containing lower bound, estimate, and upper bound. If multiple sets of
#' estimates are required, they can be passed as additional arguments, each
#' separated by commas.
#'
#' @param Initial Numeric. Initial estimate for the model parameter.
#' @param ... Additional initial estimate(s) for the model parameter.
#'
#' @return An object of class `InitialEstimate`.
#'
#' @family Thetas
#'
#' @examples
#' InitialEstimate(1)
#' InitialEstimate(c(0, 1, Inf), c(-Inf, 2, 10))
#'
#' @export
InitialEstimate <- function(Initial = numeric(),
                            ...) {
  if (!missing(Initial)) {
    EstimateList <- .extract_EstimateFromInitial(Initial)
  } else {
    EstimateList <- list(Lower = numeric(0),
                         Estimate = numeric(0),
                         Upper = numeric(0))
  }

  dots <- eval(substitute(alist(...), env = environment()))
  if (length(dots) > 0) {
    for (dotsEstimate in dots) {
      EstimateListDots <- .extract_EstimateFromInitial(eval(dotsEstimate))
      EstimateList$Lower <- c(EstimateList$Lower, EstimateListDots$Lower)
      EstimateList$Estimate <- c(EstimateList$Estimate, EstimateListDots$Estimate)
      EstimateList$Upper <- c(EstimateList$Upper, EstimateListDots$Upper)
    }
  } else if (length(unlist(EstimateList)) == 0) {
    stop("No initial estimates provided. Please provide at least one initial estimate.")
  }

  InitialEstimateInstance <- new_InitialEstimate(Lower = EstimateList$Lower,
                                                 Estimate = EstimateList$Estimate,
                                                 Upper = EstimateList$Upper)

  validate_InitialEstimate(InitialEstimateInstance)
}
