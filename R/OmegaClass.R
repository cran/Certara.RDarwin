#' Create a new Omega instance
#'
#' This function creates a new Omega instance with the given parameters.
#'
#' @inheritParams Omega
#'
#' @return A new Omega instance.
#' @noRd
#' @keywords internal
new_Omega <- function(Name = character(),
                      InitialOmega = numeric(),
                      State = c("Present", "None", "Searched"),
                      Frozen = FALSE,
                      StParmName = character(),
                      PMLStructure = character()) {
  State <- match.arg(State)
  if (State == "None") {
    return(structure(
      list(
        Name = Name,
        InitialOmega = InitialOmega,
        State = State,
        Frozen = Frozen,
        StParmName = StParmName,
        PMLStructure = PMLStructure
      ),
      class = "Omega"
    ))
  }

  if ((!methods::hasArg(Name) | length(Name) == 0)) {
    if (!methods::hasArg(StParmName) | length(StParmName) == 0) {
      stop("No Name for Omega is given")
    }

    Name <- paste0("n", StParmName)
  }

  stopifnot(length(Name) == 1 &
              length(InitialOmega) == 1 &
              length(State) == 1 &
              length(Frozen) == 1)

  stopifnot(is.character(Name) & .check_0nzchar(Name))
  stopifnot(is.character(StParmName))
  stopifnot(is.character(PMLStructure))

  stopifnot(is.logical(Frozen))
  stopifnot(is.numeric(InitialOmega))

  structure(
    list(
      Name = Name,
      InitialOmega = InitialOmega,
      State = State,
      Frozen = Frozen,
      StParmName = StParmName,
      PMLStructure = PMLStructure
    ),
    class = "Omega"
  )
}

#' Validate an Omega instance
#'
#' This function validates the given Omega instance and checks if its initial value is not NA and not less or equal to 0.
#'
#' @param OmegaInstance An Omega instance to be validated.
#'
#' @return The validated Omega instance.
#' @noRd
#' @keywords internal
validate_Omega <- function(OmegaInstance) {
  if (any(is.na(OmegaInstance$InitialOmega))) {
    stop("Omega cannot be NA:", paste(OmegaInstance$InitialOmega))
  }

  if (any(OmegaInstance$InitialOmega <= 0)) {
    stop("Omega cannot be less or equal to 0: ",
         paste(OmegaInstance$InitialOmega))
  }

  if (length(OmegaInstance$PMLStructure) > 1) {
    stop("More than 1 'PMLStructure' provided for Omega ", OmegaInstance$Name)
  }

  OmegaInstance
}

#' Create an Omega instance with validation
#'
#' This function creates an Omega instance with the given parameters and validates it.
#'
#' @param Name A character string specifying the name of the Omega.
#' @param InitialOmega Numeric specifying the initial value of the Omega. Default value is 1.
#' @param State Character specifying the presence of the Omega. Possible values are:
#' \itemize{
#'   \item `None` The Omega does not exist in the specified `PMLStructures`.
#'   \item `Present` The Omega exists in the specified `PMLStructures` (the default).
#'   \item `Searched` The presence of the Omega is searched.
#' }
#' @param Frozen A logical value indicating whether the Omega is frozen or not.
#' @param StParmName A character string specifying the corresponding structural parameter name.
#' @param PMLStructure PML structure current omega belongs to.
#'
#' @return An Omega instance.
#'
#' @family Omegas
#' @seealso [list_Omegas()]
#'
#' @examples
#' nV <- Omega("nV")
#'
#' @export
Omega <- function(Name = character(),
                  InitialOmega = 1,
                  State = "Present",
                  Frozen = FALSE,
                  StParmName = character(),
                  PMLStructure = character()) {
  if (!.check_0nzchar(Name) & .check_0nzchar(StParmName)) {
    Name <- paste0("n", StParmName)
  }

  OmegaInstance <- new_Omega(
    Name = Name,
    InitialOmega = InitialOmega,
    State = State,
    Frozen = Frozen,
    StParmName = StParmName,
    PMLStructure = PMLStructure
  )

  validate_Omega(OmegaInstance)
}

#' @export
print.Omega <- function(x, ...) {
  on.exit(clean_TokensEnv(e = TokensEnv))
  cat(output(x), "\n")
}

#' @noRd
#' @keywords internal NLME
output.Omega <- function(x, ...) {
  if (x$Frozen) {
    FreezePart <- "(freeze) "
  } else {
    FreezePart <- ""
  }

  RanefText <-
    paste0("ranef(diag(",
           x$Name,
           ") ",
           FreezePart,
           "= c(",
           x$InitialOmega,
           "))")
  if (x$State == "Present") {
    RanefText
  } else if (x$State == "Searched") {
    add_TokensNLME(
      TokenName = paste(
        "", x$Name, sep = "_"),
      ListElementName = "OmegaInit",
      TokenValues = c("", RanefText),
      DoNotChangeTokenListMain = FALSE
    )
  } else {
    character(0)
  }
}
