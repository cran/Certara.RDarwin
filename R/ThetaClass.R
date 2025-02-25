#' Create a new Theta instance.
#'
#' @inheritParams Theta
#'
#' @return A Theta instance.
#'
#' @examples
#' # Create a new Theta instance
#' theta <- new_Theta(Name = "theta1", InitialEstimates = c(0, 1, NA), Frozen = FALSE)
#' @noRd
#' @keywords internal
new_Theta <- function(Name = character(),
                      InitialEstimates = c(),
                      State = c("Present", "None", "Searched"),
                      Frozen = FALSE,
                      StParmName = character(),
                      PMLStructure = character()) {
  State <- match.arg(State)
  if (State == "None") {
    return(structure(
      list(
        Name = Name,
        InitialEstimates = InitialEstimates,
        State = State,
        Frozen = Frozen,
        StParmName = StParmName,
        PMLStructure = PMLStructure
      ),
      class = "Theta"
    ))
  }

  if ((!methods::hasArg(Name) | length(Name) == 0)) {
    if (!methods::hasArg(StParmName) | length(StParmName) == 0) {
      stop("No Name for Theta is given")
    }

    Name <- paste0("tv", StParmName)
  }

  stopifnot(
    length(Name) <= 1 &
      length(Frozen) == 1 &
      length(InitialEstimates) >= 1 &
      length(State) == 1
  )

  stopifnot(is.character(Name))
  stopifnot(is.logical(Frozen))
  stopifnot(.check_0nzchar(Name) | .check_0nzchar(StParmName))
  stopifnot(is.character(PMLStructure))

  if (!.check_0nzchar(Name))
    Name <- paste0("tv", StParmName)

  structure(
    list(
      Name = Name,
      InitialEstimates = InitialEstimates,
      State = State,
      Frozen = Frozen,
      StParmName = StParmName,
      PMLStructure = PMLStructure
    ),
    class = "Theta"
  )
}

#' Validate a Theta instance
#'
#' This function validates the given Theta instance.
#'
#' @param ThetaInstance A Theta instance to be validated.
#'
#' @return The validated Theta instance.
#' @noRd
#' @keywords internal
validate_Theta <- function(ThetaInstance) {
  if (length(ThetaInstance$PMLStructure) > 1) {
    stop("More than 1 'PMLStructure' provided for Theta ",
         ThetaInstance$Name)
  }

  ThetaInstance
}

#' returns FALSE if any character element is empty or ""
#' otherwise TRUE
#' @noRd
#' @keywords internal
.check_0nzchar <- function(String) {
  if (any(!length(String))) {
    FALSE
  } else {
    any(nzchar(String))
  }
}

#' Create a new Theta instance with validation.
#'
#' @param Name A character string representing the name of the Theta instance.
#' @param InitialEstimates An [InitialEstimate()] class instance or a numerical
#'   value for the initial estimate of the Theta or a numeric vector length
#'   three with its elements representing the lower bound, initial estimate.
#' @param State Character specifying the presence of the Theta. Possible values are:
#' * `None` The Theta does not exist in the specified `PMLStructure`.
#' * `Present` The Theta exists in the specified `PMLStructure` (the default)
#' * `Searched` The presence of the Theta is searched.
#' @param Frozen A logical value indicating whether the Theta will be estimated or not.
#' @param StParmName A character specifying the corresponding structural
#'   parameter name. Used for the `Name` of current Theta construction if it is not specified as 'tv' + `StParmName`.
#' @param PMLStructure PML structure current theta belongs to
#'
#' @return A Theta instance.
#'
#' @seealso [InitialEstimate()] [StParm()]
#' @family Thetas
#'
#' @examples
#' # Create a new Theta instance with a name 'tvV' and initial value 2 (no bounds)
#' theta <- Theta(Name = "tvV", InitialEstimates = 2)
#'
#' @export
Theta <- function(Name = character(),
                  InitialEstimates = 1,
                  State = "Present",
                  Frozen = FALSE,
                  StParmName = character(),
                  PMLStructure = character()) {
  if (!inherits(InitialEstimates, "InitialEstimate")) {
    InitialEstimates <- InitialEstimate(InitialEstimates)
  }

  validate_Theta(
    new_Theta(
      Name = Name,
      InitialEstimates = InitialEstimates,
      State = State,
      Frozen = Frozen,
      StParmName = StParmName,
      PMLStructure = PMLStructure
    )
  )
}

#' @export
print.Theta <- function(x, ...) {
  on.exit(clean_TokensEnv(e = TokensEnv))
  cat(output(x), "\n")
}


#' @noRd
#' @keywords internal NLME
output.Theta <- function(x, ...) {
  if (x$Frozen) {
    FreezePart <- "(freeze) "
  } else {
    FreezePart <- ""
  }

  Initials <- c()
  for (nEstimate in 1:length(x$InitialEstimates$Estimate)) {
    Lower <- x$InitialEstimates$Lower[nEstimate]
    if (is.na(Lower) | is.infinite(Lower) | !is.numeric(Lower)) {
      Lower <- ""
    }

    Estimate <- x$InitialEstimates$Estimate[nEstimate]

    Upper <- x$InitialEstimates$Upper[nEstimate]
    if (is.na(x$InitialEstimates$Upper[nEstimate]) |
        is.infinite(x$InitialEstimates$Upper[nEstimate])) {
      Upper <- ""
    }

    Initial <- paste(Lower, Estimate, Upper, sep = ", ")

    Initials <- c(Initials, Initial)
  }

  if (length(Initials) == 1) {
    InitialEstimateText <- Initials
  } else {
    InitialEstimateText <-
      add_TokensNLME(
        TokenName = paste0("_InEst_", x$Name),
        ListElementName = "ThetaInit",
        TokenValues = Initials,
        DoNotChangeTokenListMain = FALSE
      )
  }

  ThetaText <-
    paste0("fixef(",
           x$Name,
           FreezePart,
           "= c(",
           InitialEstimateText,
           "))")
  if (x$State == "Present") {
    ThetaText
  } else if (x$State == "Searched") {
    add_TokensNLME(
      TokenName = paste0(x$PMLStructure, "_fixef_", x$Name),
      ListElementName = "fixef",
      TokenValues = c("", ThetaText),
      DoNotChangeTokenListMain = FALSE
    )
  } else {
    character(0)
  }
}
