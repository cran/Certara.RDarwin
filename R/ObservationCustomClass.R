#' Create a new Observation Custom object
#'
#' @inheritParams ObservationCustom
#'
#' @return A new ObservationCustom object
#'
#' @examples
#' new_ObservationCustom(ObservationName = "CObs",
#'                 Type = "observe",
#'                 Statement = "(CObs = C + CEps)",
#'                 StatementNames = list("C", "CEps"),
#'                 Sigma = list(CEps = 1),
#'                 PMLStructure = "")
#'
#' @noRd
#' @keywords internal NLME
new_ObservationCustom <- function(ObservationName = "",
                                  Type = "observe",
                            Statement = "",
                            StatementNames = list(),
                            Sigma = list(),
                            Dobefore = c(),
                            Doafter = c(),
                            BQL = FALSE,
                            BQLValue = NA,
                            PMLStructure = character()) {
  stopifnot(length(ObservationName) == 1)
  stopifnot(length(Statement) == 1)
  match.arg(Type, c("observe", "multi", "LL", "event", "count", "ordinal"))

  if (Type == "observe") {
    stopifnot(length(Sigma) == 1)
    stopifnot(!is.null(names(Sigma)))
  } else {
    stopifnot(length(Sigma) == 0)
  }

  if (length(PMLStructure) > 1) {
    stop("More than 1 'PMLStructure' provided for ObservationCustom ",
         ObservationName)
  }

  stopifnot(is.character(ObservationName) &
              .check_0nzchar(ObservationName))

  stopifnot(is.character(Statement) &
              .check_0nzchar(Statement))

  stopifnot(is.list(Sigma))

  stopifnot(is.list(StatementNames))

  stopifnot(is.character(PMLStructure))

  structure(
    list(
      ObservationName = ObservationName,
      Type = Type,
      Statement = Statement,
      StatementNames = StatementNames,
      Sigma = Sigma,
      Dobefore = Dobefore,
      Doafter = Doafter,
      BQL = BQL,
      BQLValue = BQLValue,
      PMLStructure = PMLStructure
    ),
    class = "ObservationCustom"
  )
}



validate_ObservationCustom <- function(ObservationCustomInstance) {
  if (ObservationCustomInstance$Type == "observe") {
    if (!names(ObservationCustomInstance$Sigma) %in% unlist(ObservationCustomInstance$StatementNames)) {
      warning(
        "Sigma name is not found in observation statement names:",
        unlist(ObservationCustomInstance$Sigma),
        paste(
          unlist(ObservationCustomInstance$StatementNames),
          collapse = ", "
        )
      )
    }
  }

  ObservationCustomInstance
}

#' Create an instance of custom Observation class.
#'
#' This function creates a new instance of custom Observation object and validates it.
#' All PML responses are supported (`observe`, `multi`, `LL`, `event`, `count`, `ordinal`)
#'
#' @param ObservationName A character string giving the name of the Observation.
#' @param Type One of the following: `observe`, `multi`, `LL`, `event`, `count`, `ordinal`
#' @param Statement A character string giving the RHS of response statement without `Type`.
#' @param StatementNames A character vector giving the names of variables used in the `Statement`.
#' @param Sigma a list specifying the chosen sigma value Should be given only if
#' `Type == "observe"`
#' @param Dobefore A character string specifying the sequence of operations to
#' be performed before current observation event.
#' @param Doafter A character string specifying the sequence of operations to
#' be performed after current observation event.
#' @param BQL A logical value indicating whether the dataset contains BQL values
#'   and they should be taken into account (M3 method).
#' @param BQLValue An optional numeric positive value of static LLOQ. Applicable
#'   only when BQL argument is `TRUE`. Any observed value less than or equal to
#'   that LLOQ value is treated as censored.
#' @param PMLStructure Character specifying the name of PML structure in which
#'   the observation should be added. For the naming convention of
#'   PMLStructures, see Details section of [create_ModelPK()].
#'
#' @return A new Observation object
#'
#' @family Observations
#'
ObservationCustom <- function(ObservationName = "CObs",
                              Type = "observe",
                        Statement = "",
                        StatementNames = list(),
                        Sigma = list(),
                        Dobefore = c(),
                        Doafter = c(),
                        BQL = FALSE,
                        BQLValue = NA,
                        PMLStructure = character()) {
  ObservationCustomInstance <- new_ObservationCustom(
    ObservationName = ObservationName,
    Type = Type,
    Statement = Statement,
    StatementNames = StatementNames,
    Sigma = Sigma,
    Dobefore = Dobefore,
    Doafter = Doafter,
    BQL = BQL,
    BQLValue = BQLValue,
    PMLStructure = PMLStructure
  )

  validate_ObservationCustom(ObservationCustomInstance)
}

#' @export
output.ObservationCustom <- function(x, ...) {
  x <- validate_ObservationCustom(x)
  OutputObservationParts <- paste0(x$Type, x$Statement)
  if (x$Type == "observe") {
    Sigma <- paste0("error(", names(x$Sigma), " = ", x$Sigma, ")")
    OutputObservationParts <- paste(OutputObservationParts,
                                    Sigma,
                                    sep = "\n")
  }

  OutputObservationParts
}

#' @export
print.ObservationCustom <- function(x, ...) {
  on.exit(clean_TokensEnv(e = TokensEnv))
  cat(output(x), "\n")
}
