#' Internal Constructor for Expression Class
#'
#' Creates a new `Expression` object with minimal validation. This function is
#' intended for internal package use, typically called by the user-facing
#' `Expression()`
#'
#' @inheritParams Expression
#'
#' @return A new object of class `Expression`, which is a list containing the
#'   provided arguments (`ExpressionText`, `ContainedStParms`, `State`).
#'
#' @noRd
#' @keywords internal NLME
new_Expression <- function(ExpressionText = character(),
                           ContainedStParms = list(),
                           State = c("Present", "None", "Searched")) {
  stopifnot(is.character(ExpressionText), length(ExpressionText) == 1)
  stopifnot(.check_0nzchar(ExpressionText))

  stopifnot(is.list(ContainedStParms))
  if (length(ContainedStParms) > 0) {
    if (inherits(ContainedStParms, "StParm")) {
      ContainedStParms <- list(ContainedStParms)
    } else {
      stopifnot(all(sapply(ContainedStParms, inherits, "StParm")))
    }
  }

  State <- match.arg(State)

  structure(
    list(
      ExpressionText = ExpressionText,
      ContainedStParms = ContainedStParms,
      State = State
    ),
    class = "Expression"
  )
}


#' Validate an Expression object
#'
#' @param ExpressionInstance An Expression object
#'
#' @return The validated Expression object
#' @noRd
#' @keywords internal NLME
validate_Expression <- function(ExpressionInstance) {
  parse_attempt <-
    try(parse(text = ExpressionInstance$ExpressionText, keep.source = FALSE),
        silent = TRUE)
  if (inherits(parse_attempt, "try-error")) {
    # Extract the error message from the try-error object
    error_msg <- conditionMessage(attr(parse_attempt, "condition"))
    stop(
      "Invalid syntax in 'ExpressionText': \"",
      ExpressionInstance$ExpressionText,
      "\"\n",
      "R parser reported: ",
      error_msg,
      call. = FALSE
    )
  }

  if (grepl(",", ExpressionInstance$ExpressionText, fixed = TRUE)) {
    warning(
      "Invalid syntax in 'ExpressionText': \"",
      ExpressionInstance$ExpressionText,
      "\"\n",
      "Commas (,) are generally not allowed in simple mathematical expressions for this context.",
      call. = FALSE
    )
  }

  if (length(ExpressionInstance$ContainedStParms) > 0) {
    is_StParm <-
      sapply(ExpressionInstance$ContainedStParms, inherits, "StParm")
    if (!all(is_StParm)) {
      stop(
        "Expression error:\n",
        "All elements in 'ContainedStParms' must be 'StParm' objects.",
        call. = FALSE
      )
    }

    is_Present <-
      sapply(ExpressionInstance$ContainedStParms, function(x)
        x$State == "Present")

    if (!all(is_Present)) {
      # stop(
      #   "Expression error:\n",
      #   "All StParms in 'ContainedStParms' must be with `Present` state.",
      #   call. = FALSE
      # )
    }
  }

  ExpressionInstance
}

#' Create an Expression object
#'
#' Represents a PML expression that can include text and structural parameters (StParm).
#' This is used for arguments like tlag, bioavail, duration, or rate in Dosepoint.
#'
#' @param ExpressionText Character string. The primary textual representation of
#'   the expression (e.g., "1-F", "Tlag", "Rate * exp(Effect)").
#' @param ContainedStParms List. A list containing any StParm objects that are
#'   referenced by or associated with this `ExpressionText.` The `State` of
#'   structural parameters depends on the `State` of expression (i.e.
#'   specification of `State` is not supported).
#' @param State Character string. The state of the expression and the associated
#'   structural parameters, controlling its inclusion or search status in the
#'   model. Must be one of 'Present', 'None', or 'Searched'.
#'
#' @return A new object of class `Expression`, which is a list containing the
#'   provided arguments (`ExpressionText`, `ContainedStParms`, `State`).
#'
#' @family Parameter Expressions
#' @seealso [Dosepoint()], [StParm()]
#'
#' @examples
#' PMLParametersSets <-
#' get_PMLParametersSets(CompartmentsNumber = c(1, 2, 3),
#'                       Absorption = c("First-Order"))
#' # add dosepoint
#' PMLParametersSets <-
#'   add_Dosepoint(PMLParametersSets,
#'                 DosepointName = "A1",
#'                 bioavail = Expression("1 - Fa"),
#'                 duration = Expression("Tlag",
#'                 ContainedStParms = list(StParm("Tlag"))))
#' @export
Expression <- function(ExpressionText = character(),
                       ContainedStParms = list(),
                       State = "Present") {
  if (length(ExpressionText) != 1 || !is.character(ExpressionText)) {
    stop("'ExpressionText' must be a single character string.")
  }

  ExpressionInstance <- new_Expression(
    ExpressionText = ExpressionText,
    ContainedStParms = ContainedStParms,
    State = State
  )

  validate_Expression(ExpressionInstance)
}

#' Generate output for Expression object
#'
#' This method primarily serves to allow extraction of contained text
#' during model generation.
#'
#' @param x An Expression object.
#' @param ... Additional arguments (currently unused).
#'
#' @return Returns the input object `x` itself. The main purpose is achieved
#'   by the model generation process which looks inside Expression objects
#'   for StParms. Returning the object allows potential chaining if needed later.
#'
#' @noRd
#' @keywords internal NLME
output.Expression <- function(x, ...) {
  if (x$State == "None") {
    return("")
  }

  x <- validate_Expression(x)

  x$ExpressionText
}

#' @export
print.Expression <- function(x, ...) {
  on.exit(clean_TokensEnv(e = TokensEnv))
  cat(output(x), "\n")
}
